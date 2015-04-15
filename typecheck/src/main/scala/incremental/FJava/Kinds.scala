package incremental.FJava

import incremental.Node._
import incremental.{SyntaxChecking, NodeKind, Typ}

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
case object Method extends Exp(simple(Seq(classOf[CName], classOf[Symbol], classOf[CName], classOf[CName]), cExp))
case object New extends Exp(simple(Seq(classOf[CName])) orElse simple(Seq(classOf[CName]), cExp))
case object UCast extends Exp(simple(Seq(classOf[CName]),cExp))
case object DCast extends Exp(simple(cExp))
case object SCast extends Exp(simple(cExp))
case object Invk extends Exp(simple(Seq(classOf[Symbol]),cExp, cExp))
case object TClass extends Exp(simple(cExp))



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
