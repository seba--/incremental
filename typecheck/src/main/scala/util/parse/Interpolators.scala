package util.parse

import com.github.javaparser.JavaParser
import incremental.Node._
import incremental.fjava.ProgramM

/**
  * String interpolators to easily parse Java into FJ
  * Created by oliver on 24.04.17.
  */
object Interpolators {
  implicit class FJInterpolator(val sc: StringContext) extends AnyVal {
    def jexp(args: Any*): Node = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      var buf = new StringBuffer(strings.next)
      while(strings.hasNext) {
        buf append expressions.next
        buf append strings.next
      }
      JavaToFJ.expr(JavaParser.parseExpression(buf.toString))
    }
    def java(args: Any*): Node = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      var buf = new StringBuffer(strings.next)
      while(strings.hasNext) {
        buf append expressions.next
        buf append strings.next
      }
      ProgramM(JavaToFJ(JavaParser.parse(buf.toString)):_*)
    }
  }
}
