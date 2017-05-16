package incremental.fjava

import java.io.{File, PrintWriter}
import java.util
import javax.lang.model.element.Element
import javax.tools.ToolProvider

import com.sun.source.tree.CompilationUnitTree
import com.sun.tools.javac.api.{JavacTaskImpl, JavacTool}
import com.sun.tools.javac.main.JavaCompiler
import com.sun.tools.javac.tree.JCTree
import com.sun.tools.javac.tree.JCTree.{JCClassDecl, JCCompilationUnit}
import com.sun.tools.javac.util.ListBuffer
import constraints.fjava.{ConstraintSystem, ConstraintSystemFactory, Type}
import incremental.Node.Node

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

case class JavacCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new JavacChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}


abstract class JavacChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {
  override type TError = String

  var sourceFiles: Option[Seq[File]] = None
  var task: Option[JavacTaskImpl] = None

  override def prepare(e: Node): Unit = {
    val sources =
      if (e.kind == ProgramM) compileProgramToJava(e).toMap
      else if (e.kind == ClassDec) Seq(compileClassToJava(e)).toMap
      else ???
    val src = newTmpDir()

    sourceFiles = Some(makeTemporarySourceFiles(src, sources))
    println(s"Wrote ${sources.size} javac input source files to $src/")

    task = Some(parseSourceFiles(sourceFiles.get))
  }

  override protected def typecheckImpl(e: Node): Either[Type, TError] = {
    if (!sourceFiles.isDefined)
      prepare(e)
    try {
      val elems = checkSourceFiles(task.get)
      if (elems.size == sourceFiles.get.size) {
        if (e.kind == ProgramM) Left(ProgramOK)
        else if (e.kind == ClassDec) Left(e.lits(0).asInstanceOf[CName])
        else ???
      }
      else
        Right(s"Javac rejected ${sourceFiles.get.size - elems.size} source files")
    } finally {
      sourceFiles = None
    }
  }

  def parseSourceFiles(sourceFiles: Seq[File]): JavacTaskImpl = {
    val compiler = ToolProvider.getSystemJavaCompiler().asInstanceOf[JavacTool]
    val fileManager = compiler.getStandardFileManager(null, null, null)
    val compilationUnits = fileManager.getJavaFileObjects(sourceFiles: _*)

    val options = util.Arrays.asList[String]()
    val classes = util.Arrays.asList[String]()
    val compilationTask = compiler.getTask(null, fileManager, null, options, classes, compilationUnits).asInstanceOf[JavacTaskImpl]
    compilationTask.parse()
    compilationTask.enter(null)
    compilationTask
  }
  
  def checkSourceFiles(compilationTask: JavacTaskImpl): Iterable[_ <: Element] = {
    //This is an adaption of JavacTaskImpl.analyze that just performs the attribute phase, which is sufficient for type checking
    val compiler = JavaCompiler.instance(compilationTask.getContext)
    var results = Vector[Element]()
    try {
      val queue = compiler.attribute(compiler.todo).asScala
      for (env <- queue) {
        if (env.tree.getTag() == JCTree.Tag.CLASSDEF) {
          val cdef = env.tree.asInstanceOf[JCClassDecl]
          if (cdef.sym != null)
            results = results :+ cdef.sym
        }
        else if (env.tree.getTag() == JCTree.Tag.TOPLEVEL) {
          val unit = env.tree.asInstanceOf[JCCompilationUnit]
          if (unit.packge != null)
            results = results :+ unit.packge
        }
      }
    } finally {
      compiler.log.flush()
    }
    results
  }


  /* Compile FJ to Java */

  type Code = String

  val filtered = Set('[', ']', '-')
  def name(s: Symbol): String = name(s.name)
  def name(s: String): String = s.replace(",", "$").replace("_", "$underscore$").filter(!filtered.contains(_))

  def compileProgramToJava(e: Node): Seq[(String, Code)] = e.kind match {
    case ProgramM =>
      e.kids.seq.flatMap ( sub =>
        if (sub.kind == ProgramM)
          compileProgramToJava(sub)
        else
          Seq(compileClassToJava(sub))
      )
  }

  def compileClassToJava(e: Node): (String, Code) = e.kind match {
    case ClassDec =>
      val c = name(e.lits(0).asInstanceOf[CName].x)
      val sup = name(e.lits(1).asInstanceOf[CName].x)
      val ctor = e.lits(2).asInstanceOf[Ctor]
      val fields = e.lits(3).asInstanceOf[Seq[(Symbol, CName)]]
      val methods = e.kids.seq

      val code =
        s"""public class $c extends $sup {
           |  ${fields.map(p => "public " + compileParam(p).toString + ";\n  ").mkString}
           |
           |  public $c(${compileParams(ctor.superParams ++ ctor.fields)}) {
           |    super(${ctor.superParams.map(s => name(s._1)).mkString(", ")});
           |    ${compileFieldInits(ctor.fields, fields)}
           |  }
           |
           |  ${methods.map(compileMethod(_)).mkString("\n\n  ")}
           |}
         """.stripMargin

      c -> code
  }

  def compileMethod(e: Node): Code = e.kind match {
    case MethodDec =>
      val retT = name(e.lits(0).asInstanceOf[CName].x) // return type
      val m = name(e.lits(1).asInstanceOf[Symbol]) // method name
      val params = e.lits(2).asInstanceOf[Seq[(Symbol, CName)]]

      s"""public $retT $m(${compileParams(params)}) {
         |    return ${compileExp(e.kids(0))};
         |  }
       """.stripMargin
  }

  def compileExps(es: Seq[Node]): Code = es.map(compileExp(_)).mkString(", ")

  def compileExp(e: Node): Code = e.kind match {
    case Var =>
      val x = name(e.lits(0).asInstanceOf[Symbol])
      s"$x"
    case FieldAcc =>
      val f = name(e.lits(0).asInstanceOf[Symbol]) //symbol
      s"${compileExp(e.kids(0))}.$f"
    case Invk =>
      val m = name(e.lits(0).asInstanceOf[Symbol])
      s"${compileExp(e.kids(0))}.$m(${compileExps(e.kids.seq.tail)})"
    case New =>
      val c = name(e.lits(0).asInstanceOf[CName].x)
      s"new $c(${compileExps(e.kids.seq)})"
    case UCast | DCast | SCast =>
      val c = name(e.lits(0).asInstanceOf[CName].x)
      s"($c) ${compileExp(e.kids(0))}"
  }

  def compileParams(ps: Iterable[(Symbol, CName)]): Code = ps.map(compileParam(_)).mkString(", ")

  def compileParam(p: (Symbol, CName)): Code = {
    val par = name(p._1)
    val typ = name(p._2.x)
    s"$typ $par"
  }

  def compileFieldInits(ctorParams: ListMap[Symbol, CName], fields: Seq[(Symbol, CName)]): Code = {
    val zipped: Seq[((Symbol, CName), (Symbol, CName))] = ctorParams.toSeq.zip(fields)
    val stmts = zipped.map {
      case ((param, _), (field, _)) => s"this.${name(field)} = ${name(param)};"
    }
    stmts.mkString("\n    ")
  }




  /* File handling */

  def newTmpDir(): File = {
    val f = File.createTempFile("JavaFiles", null)
    f.delete()
    f.mkdir()
    f
  }

  def writeTemporarySourceFile(dir: File, unitName: String, sourceCode: String) = {
    val sourceFile = new File(dir, s"$unitName.java")
    val printWriter = new PrintWriter(sourceFile)
    try {
      printWriter.write(sourceCode)
    } finally {
      printWriter.close()
    }
    sourceFile
  }

  def makeTemporarySourceFiles(dir: File, sourceFileCodes /* name -> code*/ : Map[String, String]): Seq[File] = {
    var sourceFiles = Seq[File]()
    for ((name, code) <- sourceFileCodes)
      sourceFiles = sourceFiles :+ writeTemporarySourceFile(dir, name, code)
    sourceFiles
  }
}
