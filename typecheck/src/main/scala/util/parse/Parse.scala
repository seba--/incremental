package util.parse


import com.github.javaparser.JavaParser
import better.files._
import com.github.javaparser.ast.{Node => ParseNode}


/**
  * Created by oliver on 20.04.17.
  */
object Parse extends App {
  val cu = JavaParser.parse(File("src/main/java/redblack/RBNode.java").toJava)
  cu.accept(new MyVisitor, ())
}


class MyVisitor extends NodeVisitor with FoldVisitor[Int] with PrintlnVisitor {
  def init(): Int = 0

  def fold(n: ParseNode, s: Int): Int = {
    println(s)
    s + 1
  }
}
