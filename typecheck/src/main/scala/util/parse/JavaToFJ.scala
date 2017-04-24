package util.parse

import com.github.javaparser.ParseException
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, Parameter}
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt.{ExplicitConstructorInvocationStmt, ExpressionStmt, ReturnStmt, Statement}
import com.github.javaparser.ast.{CompilationUnit, NodeList}
import incremental.Node
import incremental.fjava._

import scala.collection.immutable.ListMap
import scala.collection.mutable

/**
  * Created by oliver on 24.04.17.
  */
object JavaToFJ extends (CompilationUnit => Seq[Node.Node]) {
  import Node._

  import collection.JavaConverters._
  import collection.immutable.Seq
  import scala.compat.java8.OptionConverters._

  type S[T] = collection.immutable.Seq[T]

  def apply(cu: CompilationUnit): Seq[Node] = {
    val types = cu.getTypes.iterator.asScala
    val classes = types map {
      case c: ClassOrInterfaceDeclaration =>
        clazz(c)
    }
    classes.to[S]
  }

  def clazz(c: ClassOrInterfaceDeclaration): Node = {
    val flds = fields(c)
    ClassDec(CName(c.getNameAsString),
             superClass(c),
             ctor(c, flds),
             flds,
             methods(c))
  }

  def fields(clazz: ClassOrInterfaceDeclaration): Seq[(Symbol, CName)] = {
    val fields = for {decl <- clazz.getFields.asScala
                      field <- decl.getVariables.asScala}
                 yield (Symbol(field.getNameAsString),
                        desugar(field.getType))
    fields.to[S]
  }

  def methods(clazz: ClassOrInterfaceDeclaration): Seq[Node] = {
    val parseMethods = clazz.getMethods.asScala
    val methods = parseMethods map { m =>
      val name = Symbol(m.getNameAsString)
      val returnType = desugar(m.getType)
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
    (Symbol(p.getNameAsString), desugar(p.getType))
  ).to[S]

  def desugar(t: Type): CName = {
    val res = Symbol(t.asString)
    CName(res match {
      case 'Block => 'Object
      case x => x
    })
  }

  def expr(e: Expression): Node = e match {
    case nu: ObjectCreationExpr =>
      val args: mutable.Buffer[Any] = nu.getArguments.asScala.map(expr)
      args.prepend(desugar(nu.getType))
      New(args:_*)
    case cast: CastExpr =>
      DCast(desugar(cast.getType), expr(cast.getExpression))
    case lambda: LambdaExpr =>
      expr(lambda.getExpressionBody.asScala.get)
    case fa: FieldAccessExpr =>
      val receiver = fa.getScope.asScala.map(expr).getOrElse(Var('this))
      FieldAcc(Symbol(fa.getNameAsString), receiver)
    case mc: MethodCallExpr if mc.getNameAsString == "execute" =>
      mc.getScope.asScala.map(expr).getOrElse(Var('this))
    case mc: MethodCallExpr =>
      val receiver = mc.getScope.asScala.map(expr).getOrElse(Var('this))
      val args: mutable.Buffer[Any] = mc.getArguments.asScala.map(expr)
      args.prepend(receiver)
      args.prepend(Symbol(mc.getNameAsString))
      Invk(args:_*)
    case name: NameExpr =>
      Var(Symbol(name.getNameAsString))
    case self: ThisExpr =>
      Var('this)
    case _ =>
      throw new ParseException(s"Unsupported expression ${e.getClass.getSimpleName}: $e")
  }

  def superClass(clazz: ClassOrInterfaceDeclaration): CName = {
    val supers = clazz.getExtendedTypes
    if (supers.isEmpty)
      CName('Object)
    else
      desugar(supers.get(0))
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
    import util.CastToOption._
    def unapply(s: Statement): Option[(Symbol, Symbol)] =
      for {
        e <- s.as[ExpressionStmt]
        asgn <- e.as[AssignExpr]
        fa <- asgn.getTarget.as[FieldAccessExpr]
        rcv <- fa.getScope.asScala
        _ <- rcv.as[ThisExpr]
        n <- asgn.getValue.as[NameExpr]
      } yield (Symbol(fa.getNameAsString), Symbol(n.getNameAsString))
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
