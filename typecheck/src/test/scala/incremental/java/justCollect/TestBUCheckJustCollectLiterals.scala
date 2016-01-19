package incremental.java.justCollect

import incremental.Node._
import incremental.java.syntax.expr._

/**
 * Created by qwert on 19.01.16.
 */
class TestBUCheckJustCollectLiterals extends TestBUCheckJustCollect {
  genConstraintTest("17", Lit(Deci("17")))(Seq())
  genConstraintTest("True", Lit(Bool(True())))(Seq())
  genConstraintTest("1.0", Lit(Deci("1.0")))(Seq())
  genConstraintTest("c", Lit(Char("c")))(Seq())
  genConstraintTest("foo", Lit(StringL("foo")))(Seq())
  genConstraintTest("Null", Lit(Null()))(Seq())
  genConstraintTest("this", This())(Seq())
}
