package util.parse

import com.github.javaparser.{JavaParser, ParseException}
import better.files._
import incremental.Node._
import incremental.fjava.ProgramM

/**
  * Created by oliver on 20.04.17.
  */
object Parse extends App {
  val dir = "src"/"main"/"java"/"redblack"
  val nodes = dir.glob("**/*.java").filter(!_.nameWithoutExtension.endsWith("Test")).flatMap { f =>
    println(f.path)
    JavaToFJ(JavaParser.parse(f.toJava))
  }
  println(ProgramM(nodes.toSeq:_*))
  
//  val cu = JavaParser.parse(File("src/main/java/redblack/RBNode.java").toJava)
//  cu.accept(new NodeVisitor with PrintlnVisitor, ())
//  println(JavaToFJ(cu))
}
