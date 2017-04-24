package util.parse

import com.github.javaparser.{JavaParser, ParseException}
import better.files._


/**
  * Created by oliver on 20.04.17.
  */
object Parse extends App {
  val cu = JavaParser.parse(File("src/main/java/redblack/RBNode.java").toJava)
  cu.accept(new NodeVisitor with PrintlnVisitor, ())
  println(JavaToFJ(cu))
}
