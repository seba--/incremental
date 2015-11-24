package incremental.java

import incremental.Context
import incremental.Node._
import incremental.java.syntax._
import incremental.java.syntax.expr.Lit
import incremental.java.syntax.expr._

import scala.Null

//import incremental.SyntaxChecking.SyntaxChecker.SyntaxError
import org.scalatest.FunSuite

/**
 * Created by qwert on 24.11.15.
 */
class MiscTests extends FunSuite {

  def addToCollectionTest[t](coll: Context[t], elem: t): Unit = {
    test (s"AddToCollection: ${coll.getConstraints} + $elem") {
      val initial = coll.getConstraints
      coll.addConstraint(elem)

      if(coll.getConstraints != elem +: initial)
        fail()
    }
  }

  def addSeqToCollectionTest[t](coll: Context[t], elems: Seq[t]): Unit = {
    test (s"AddSeqToCollection: ${coll.getConstraints} + $elems") {
      val initial = coll.getConstraints
      coll.addConstraintSeq(elems)

      if(coll.getConstraints != elems ++ initial)
        fail()
    }
  }

  def collectionTest(): Unit = {
    test (s"Collection Test: ") {
      val ctx: Context[Int] = new Context[Int]

      ctx.addConstraint(9)
      ctx.addConstraint(8)
      ctx.addConstraints(5, 6, 7)
      ctx.addConstraintSeq(Seq(1, 2, 3, 4))

      if(ctx.getConstraints != Seq(1, 2, 3, 4, 5, 6, 7, 8, 9))
        fail()
    }
  }

  collectionTest()

  val intCollection = new Context[Int]

  addToCollectionTest(intCollection, 1)
  addSeqToCollectionTest(intCollection, Seq(2, 3, 4, 5))

  intCollection.addConstraint(1)
  intCollection.addConstraints(2, 3, 4, 5)

  addToCollectionTest(intCollection, 0)
}
