package util.parse

import java.util

import incremental._
import com.github.javaparser.{JavaParser, ParseException}
import better.files._
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._

import scala.collection.immutable.ListMap
import com.github.javaparser.ast.{CompilationUnit, NodeList}
import incremental.fjava._

import scala.collection.mutable

/**
  * Created by oliver on 20.04.17.
  */
object Parse extends App {
  val cu = JavaParser.parse(File("src/main/java/redblack/RBNode.java").toJava)
  cu.accept(new MyVisitor, ())
  println(JavaToFJ(cu))
}

object JavaToFJ extends (CompilationUnit => Seq[Node.Node]) {
  import collection.JavaConverters._
  import collection.immutable.Seq
  import Node._
  import scala.compat.java8.OptionConverters._

  type S[T] = collection.immutable.Seq[T]
  
  def apply(cu: CompilationUnit): Seq[Node] = {
    val types = cu.getTypes.iterator.asScala
    val classes = types map {
      case clazz: ClassOrInterfaceDeclaration =>
        val flds = fields(clazz)
        ClassDec(CName(clazz.getNameAsString),
                 superClass(clazz),
                 ctor(clazz, flds),
                 flds,
                 methods(clazz))
    }
    classes.to[S]
  }

  def fields(clazz: ClassOrInterfaceDeclaration): Seq[(Symbol, CName)] = {
    val fields = for {decl <- clazz.getFields.asScala
                      field <- decl.getVariables.asScala}
                 yield (Symbol(field.getNameAsString),
                        CName(field.getType.asString))
    fields.to[S]
  }

  def methods(clazz: ClassOrInterfaceDeclaration): Seq[Node] = {
    val parseMethods = clazz.getMethods.asScala
    val methods = parseMethods map { m =>
      val name = Symbol(m.getNameAsString)
      val returnType = CName(m.getType.asString)
      val ps = params(m.getParameters)
      val body = m.getBody.get.getStatement(0) match {
        case ret: ReturnStmt =>
          expr(ret.getExpression.get)
        case _ =>
          throw new ParseException(s"Method $name in class ${clazz.getNameAsString}: Method bodies must consist of a single return statement")
      }
      MethodDec(returnType, name, ps, body)
    }
    methods.to[S]
  }

  def params(ps: NodeList[Parameter]): Seq[(Symbol, CName)] = ps.asScala.map( p =>
    (Symbol(p.getNameAsString), CName(p.getType.asString))
  ).to[S]


  def expr(e: Expression): Node = e match {
    case nu: ObjectCreationExpr =>
      val args = nu.getArguments.asScala.map(expr).to[S]
      New(CName(nu.getType.getNameAsString), args:_*)
    case cast: CastExpr =>
      SCast(CName(cast.getType.asString), expr(cast.getExpression)) //TODO correct AST class?
    case lambda: LambdaExpr => ???

    case fa: FieldAccessExpr =>
      val receiver = fa.getScope.asScala.map(expr).getOrElse(Var('this))
      FieldAcc(Symbol(fa.getNameAsString), receiver)
    case mc: MethodCallExpr =>
      val receiver = mc.getScope.asScala.map(expr).getOrElse(Var('this))
      val args = mc.getArguments.asScala.map(expr)
      args.prepend(receiver)
      Invk(Symbol(mc.getNameAsString), args.to[S]:_*)
    case name: NameExpr =>
      Var(Symbol(name.getNameAsString))
    case _ =>
      throw new ParseException(s"Unsupported expression $e")
  }

  def superClass(clazz: ClassOrInterfaceDeclaration): CName = {
    val supers = clazz.getExtendedTypes
    if (supers.isEmpty)
      CName('Object)
    else
      CName(Symbol(supers.get(0).getNameAsString))
  }

  def checkArgsCorrespond(params: ListMap[Symbol, CName], args: mutable.Buffer[Expression]): Boolean = {
    params.size == args.size && params.zip(args).forall {
      case ((x,_), e: NameExpr) =>
        e.getNameAsString == x.name
      case _ => false
    }
  }

  /**
    * Extractor that matches statements of the form 'this.x = y'.
    */
  object FieldInitialization {
    def unapply(s: Statement): Option[(Symbol, Symbol)] = s match {
      case e: ExpressionStmt =>
        e.getExpression match {
          case asgn: AssignExpr =>
            asgn.getTarget match {
              case fa: FieldAccessExpr =>
                fa.getScope.asScala match {
                  case Some(_: ThisExpr) =>
                    asgn.getValue match {
                      case n: NameExpr =>
                        Some(Symbol(fa.getNameAsString), Symbol(n.getNameAsString))
                      case _ => None
                    }
                  case _ => None
                }
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }
  }
  
  def ctor(clazz: ClassOrInterfaceDeclaration, fields: Seq[(Symbol, CName)]): Ctor = {
    if (clazz.getDefaultConstructor.isPresent) {
      val ctor = clazz.getDefaultConstructor.get
      val ps = ListMap(params(ctor.getParameters):_*)
      var body = ctor.getBody.getStatements.asScala
      //check for optional super call as first body statement
      val nsuper = body.headOption match {
        case Some(supr: ExplicitConstructorInvocationStmt) =>
          val n = supr.getArguments.size
          val superparams = ps.take(n)
          val args = supr.getArguments.asScala
          if (!checkArgsCorrespond(superparams, args))
            throw new ParseException(s"Constructor parameters $superparams do not match super arguments $args")
          body = body.tail
          n
        case _ => 0
      }
      //rest of ctor body statements must assign remaining parameters to fields in their definition order
      if (body.size != fields.size)
        throw new ParseException(s"Constructor $ctor of class ${clazz.getNameAsString} must initialize all declared fields")
      val bodyOk = (fields zip body).corresponds(ps.drop(nsuper).toSeq) {
        case (((field1, _), FieldInitialization(field2, x)), (y, _)) =>
          field1 == field2 && field2 == x && x == y
        case _ => false
      }
      if (!bodyOk)
        throw new ParseException(s"Constructor $ctor of class ${clazz.getNameAsString}: Names, number and order of field parameters must match field declarations")
      Ctor(ps.take(nsuper), ps.drop(nsuper))
    }
    else Ctor(ListMap(), ListMap())
  }
}


class MyVisitor extends NodeVisitor with PrintlnVisitor
