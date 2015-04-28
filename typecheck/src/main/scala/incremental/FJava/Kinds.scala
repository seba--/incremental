package incremental.FJava

import incremental.Node._
import incremental.{Node_, SyntaxChecking, NodeKind, Typ}

/**
 * Created by lirakuci on 3/10/15.
 */

abstract class Exp(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Exp {
  val cExp = classOf[Exp]
}
import Exp._
case object Var  extends Exp(simple(Seq(classOf[Symbol])))
case object Field extends Exp(simple(Seq(classOf[Symbol]), cExp))
case object Method extends Exp(simple(Seq(classOf[CName], classOf[Symbol], classOf[Symbol], classOf[CName]), cExp))
case object New extends Exp(_ => NewSyntax)
case object UCast extends Exp(simple(Seq(classOf[CName]),cExp))
case object DCast extends Exp(simple(cExp))
case object SCast extends Exp(simple(cExp))
//case object Invk extends Exp((Seq(classOf[Symbol]),Seq(cExp)))
case object Invk extends Exp(_ =>InvkSyntax)
case object TClass extends Exp(simple(cExp))

object InvkSyntax extends SyntaxChecking.SyntaxChecker(Invk) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
  }
}

object NewSyntax extends SyntaxChecking.SyntaxChecker(New) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
  }
}
    //if (kids.size == -1)
      //error(s"No kinds found")

   // if (lits.size != kids.size)
     // error(s"Mismatching number of record labels (${lits.size}}) and initializing expressions (${kids.size}})")


//case object Num  extends Exp(simple(Seq(classOf[Integer])))
//case object Add  extends Exp(simple(cExp, cExp))
//case object Mul  extends Exp(simple(cExp, cExp))
//case object Var  extends Exp(simple(Seq(classOf[Symbol])))
//case object Abs  extends Exp(simple(Seq(classOf[Symbol]), cExp) orElse simple(Seq(classOf[Symbol], classOf[Typ[_]]), cExp))
//case object TAbs extends Exp(simple(Seq(classOf[Symbol]), cExp))
//case object App  extends Exp(simple(cExp, cExp))
//case object TApp extends Exp(simple(Seq(classOf[Typ[_]]), cExp))
//case object If0  extends Exp(simple(cExp, cExp, cExp))
//case object Fix  extends Exp(simple(cExp))
