package incremental.fjava

import java.io.{File, PrintWriter}
import java.util
import javax.lang.model.element.Element
import javax.tools.ToolProvider

import com.sun.tools.javac.api.JavacTool
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

  override def prepare(e: Node): Unit = {
    val sources =
      if (e.kind == ProgramM) compileProgramToJava(e).toMap
      else if (e.kind == ClassDec) Seq(compileClassToJava(e)).toMap
      else ???
    val src = newTmpDir()

    sourceFiles = Some(makeTemporarySourceFiles(src, sources))
    println(s"Wrote ${sources.size} javac input source files to $src/")
  }

  override protected def typecheckImpl(e: Node): Either[Type, TError] = {
    if (!sourceFiles.isDefined)
      prepare(e)
    try {
      val elems = checkSourceFiles(sourceFiles.get)
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

  def checkSourceFiles(sourceFiles: Seq[File]): Iterable[_ <: Element] = {
    val compiler = ToolProvider.getSystemJavaCompiler().asInstanceOf[JavacTool]
    val fileManager = compiler.getStandardFileManager(null, null, null)
    val compilationUnits = fileManager.getJavaFileObjects(sourceFiles: _*)

    val options = util.Arrays.asList[String]()
    val classes = util.Arrays.asList[String]()
    val compilationTask = compiler.getTask(null, fileManager, null, options, classes, compilationUnits).asInstanceOf[com.sun.tools.javac.api.JavacTaskImpl]

    val result = compilationTask.analyze().asScala
    result
  }


  /* Compile FJ to Java */

  type Code = String

  def compileProgramToJava(e: Node): Seq[(CName, Code)] = e.kind match {
    case ProgramM =>
      e.kids.seq.map(compileClassToJava(_))
  }

  def compileClassToJava(e: Node): (CName, Code) = e.kind match {
    case ClassDec =>
      val c = e.lits(0).asInstanceOf[CName]
      val sup = e.lits(1).asInstanceOf[CName]
      val ctor = e.lits(2).asInstanceOf[Ctor]
      val fields = e.lits(3).asInstanceOf[Seq[(Symbol, CName)]]
      val methods = e.kids.seq

      val code =
        s"""public class $c extends $sup {
           |  ${fields.map(p => "public " + compileParam(p).toString + ";\n  ").mkString}
           |
           |  public $c(${compileParams(ctor.superParams ++ ctor.fields)}) {
           |    super(${ctor.superParams.map(_._1.name).mkString(", ")});
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
      val retT = e.lits(0).asInstanceOf[CName] // return type
      val m = e.lits(1).asInstanceOf[Symbol].name // method name
      val params = e.lits(2).asInstanceOf[Seq[(Symbol, CName)]]

      s"""public $retT $m(${compileParams(params)}) {
         |    return ${compileExp(e.kids(0))};
         |  }
       """.stripMargin
  }

  def compileExps(es: Seq[Node]): Code = es.map(compileExp(_)).mkString(", ")

  def compileExp(e: Node): Code = e.kind match {
    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol].name
      s"$x"
    case FieldAcc =>
      val f = e.lits(0).asInstanceOf[Symbol].name //symbol
      s"${compileExp(e.kids(0))}.$f"
    case Invk =>
      val m = e.lits(0).asInstanceOf[Symbol].name
      s"${compileExp(e.kids(0))}.$m(${compileExps(e.kids.seq.tail)})"
    case New =>
      val c = e.lits(0).asInstanceOf[CName]
      s"new $c(${compileExps(e.kids.seq)})"
    case UCast | DCast | SCast =>
      val c = e.lits(0).asInstanceOf[CName]
      s"($c) ${compileExp(e.kids(0))}"
  }

  def compileParams(ps: Iterable[(Symbol, CName)]): Code = ps.map(compileParam(_)).mkString(", ")

  def compileParam(p: (Symbol, CName)): Code = {
    val name = p._1.name
    val typ = p._2
    s"$typ $name"
  }

  def compileFieldInits(ctorParams: ListMap[Symbol, CName], fields: Seq[(Symbol, CName)]): Code = {
    val zipped: Seq[((Symbol, CName), (Symbol, CName))] = ctorParams.toSeq.zip(fields)
    val stmts = zipped.map {
      case ((param, _), (field, _)) => s"this.${field.name} = ${param.name};"
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

  def writeTemporarySourceFile(dir: File, unitName: CName, sourceCode: String) = {
    val sourceFile = new File(dir, s"$unitName.java")
    val printWriter = new PrintWriter(sourceFile)
    try {
      printWriter.write(sourceCode)
    } finally {
      printWriter.close()
    }
    sourceFile
  }

  def makeTemporarySourceFiles(dir: File, sourceFileCodes /* name -> code*/ : Map[CName, String]): Seq[File] = {
    var sourceFiles = Seq[File]()
    for ((name, code) <- sourceFileCodes)
      sourceFiles = sourceFiles :+ writeTemporarySourceFile(dir, name, code)
    sourceFiles
  }
}
