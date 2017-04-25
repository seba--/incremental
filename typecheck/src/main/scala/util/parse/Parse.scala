package util.parse

import com.github.javaparser.{JavaParser, ParseException}
import better.files._
import incremental.Node._
import incremental.fjava.ProgramM

/**
  * Created by oliver on 20.04.17.
  */
object Parse extends App {
  val dir = "src"/"main"/"java"
  val nodes = dir.glob("**/*.java").filter(f => !f.nameWithoutExtension.endsWith("Test") && f.nameWithoutExtension != "Block").flatMap { f =>
    println(f.path)
    val nodes = JavaToFJ(JavaParser.parse(f.toJava))
    val fj = f.parent/ s"${f.nameWithoutExtension}.fj"
    fj.write(nodes.mkString("\n\n"))
    nodes
  }
  println(ProgramM(nodes.toSeq:_*))
  
//  val cu = JavaParser.parse(File("src/main/java/redblack/RBNode.java").toJava)
//  cu.accept(new NodeVisitor with PrintlnVisitor, ())
//  println(JavaToFJ(cu))
}
