package util.parse

import java.util

import incremental._
import com.github.javaparser.JavaParser
import better.files._
import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, ConstructorDeclaration, FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.{Node => ParseNode, _}
import com.github.javaparser.ast.stmt._

import scala.collection.immutable.Queue
import java.util.{List => JList}
import incremental.fjava._

/**
  * Created by oliver on 20.04.17.
  */
object Parse extends App {
  val cu = JavaParser.parse(File("src/main/java/redblack/RBNode.java").toJava)
  cu.accept(new MyVisitor, ())
  println(cu.accept(new JavaToFJ, ()))
}



class JavaToFJ extends NodeListVisitor[Node.Node] {
  import collection.JavaConverters._
  import collection.immutable.Seq
  type L = JList[Node.Node]

  def jlist(n: Node.Node): L = {
    val res = new util.ArrayList[Node.Node]
    res.add(n)
    res
  }

  def join(n: ParseNode, l: L): L = n match {
    //    case im: ImportDeclaration =>
    //      q
    case c: ClassOrInterfaceDeclaration =>
      l
    case ctor: ConstructorDeclaration =>
      l
    case supr: ExplicitConstructorInvocationStmt =>
      l
    case fd: FieldDeclaration =>
      l
    case md: MethodDeclaration =>
      l
    case nu: ObjectCreationExpr =>
      l
    case ret: ReturnStmt =>
      l
    case point: FieldAccessExpr =>
      l
    case asgn: AssignExpr =>
      l
    case call: MethodCallExpr =>
      l
    case lam: LambdaExpr =>
      l
    case cast: CastExpr =>
      l
    case self: ThisExpr =>
      l
    case _ =>
      l
  }
}


class MyVisitor extends NodeVisitor with PrintlnVisitor
