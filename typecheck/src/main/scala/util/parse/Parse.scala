package util.parse

import incremental._
import com.github.javaparser.JavaParser
import better.files._
import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, ConstructorDeclaration, FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.{Node => ParseNode, _}
import com.github.javaparser.ast.stmt._

import scala.collection.immutable.Queue


/**
  * Created by oliver on 20.04.17.
  */
object Parse extends App {
  val cu = JavaParser.parse(File("src/main/java/redblack/RBNode.java").toJava)
  cu.accept(new MyVisitor, ())
}


class MyVisitor extends NodeVisitor with FoldVisitor[Queue[Node.Node]] with PrintlnVisitor {
  type Q = Queue[Node.Node] 

  def init(): Q = Queue()

  def fold(n: ParseNode, q: Q): Q = n match {
    case im: ImportDeclaration =>
      q
    case c: ClassOrInterfaceDeclaration =>
      q
    case ctor: ConstructorDeclaration =>
      q
    case supr: ExplicitConstructorInvocationStmt =>
      q
    case fd: FieldDeclaration =>
      q
    case md: MethodDeclaration =>
      q
    case nu: ObjectCreationExpr =>
      q
    case ret: ReturnStmt =>
      q
    case point: FieldAccessExpr =>
      q
    case asgn: AssignExpr =>
      q
    case call: MethodCallExpr =>
      q
    case lam: LambdaExpr =>
      q
    case cast: CastExpr =>
      q
    case self: ThisExpr =>
      q
    case _ =>
      q
  }
}
