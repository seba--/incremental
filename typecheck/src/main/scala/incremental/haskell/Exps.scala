package incremental.haskell

import incremental.haskell.Node._
import incremental.haskell.SyntaxChecking.SyntaxCheck
import incremental.haskell._
import incremental.{NodeKind, Node_, SyntaxChecking}

import scala.collection.immutable.ListMap


case object Infix extends Exp(_ => InfixSyntax) {
  override def toString(e: Node_[_]): Option[String] = {
    val lexp = e.kids(0)
    val op = e.lits(0).asInstanceOf[Symbol]//TODO See this again expr1 '+ expr2
    val infixexp = e.kids(1)
    Some(s"$lexp $op $infixexp")
  }
}

abstract class Literal(syntaxcheck: SyntaxChecking.SyntaxCheck) extends Exp(syntaxcheck)
object Literal {
  val cLit = classOf[Literal]
}
case object CInt  extends Literal(simple(Seq(classOf[Integer])))
case object CFloat  extends Literal(simple(Seq(classOf[String])))
case object CDouble  extends Literal(simple(Seq(classOf[String])))
case object CRational  extends Literal(simple(Seq(classOf[String])))
case object CChar  extends Literal(simple(Seq(classOf[Symbol])))
case object CString  extends Literal(simple(Seq(classOf[String])))



abstract class Exp(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Exp {
  val cExp = classOf[Exp]
}
import Exp._
import Literal._
import incremental.haskell.OptionKind.option
import incremental.haskell.Pat._
import incremental.haskell.Toplevel._
import incremental.haskell.OptionKind._



//case object Abs  extends Exp(simple(Seq(classOf[Symbol]), cExp) orElse simple(Seq(classOf[Symbol], classOf[PolType]), cExp) orElse (simple(Seq(classOf[Seq[Symbol]]), cExp)))
//case object TAbs extends Exp(simple(Seq(classOf[Symbol]), cExp))
//case object App  extends Exp(simple(cExp, cExp))
//case object TApp extends Exp(simple(Seq(classOf[PolType]), cExp))
//case object Fix  extends Exp(simple(cExp))
//case object Let extends Exp(simple(Seq(classOf[Symbol]), cExp, cExp))
//case object Inst extends Exp(simple(Seq(classOf[Symbol], classOf[PolType]), cExp, cExp))
case object TAdd  extends Exp(simple(cExp, cExp))
case object TMul  extends Exp(simple(cExp, cExp))
case object Add  extends Exp(simple(cExp, cExp))
case object Mul  extends Exp(simple(cExp, cExp))
case object Leq  extends Exp(simple(cExp, cExp)) {
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.kids(0)} < = ${e.kids(1)}")
}


case object Var extends Exp(simple(Seq(classOf[Symbol]))) {
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.lits(0).asInstanceOf[Symbol].name}")
}
case object GConB extends Exp(_ => GConBSyntax) { //TODO See again the Types for Cons if it should be a symbol or TData (like CNAME)
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.lits(0)}(${e.kids.seq.mkString(", ")})")
}
case object GConS extends Exp(_=> GConSSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.lits(0)}[${e.kids.seq.mkString(", ")}]")
}
case object GConC extends Exp(_=> GConCSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.lits(0)}{${e.kids.seq.mkString(", ")}}")
}
case object Lit extends Exp(simple(cLit))

case object PExp extends Exp(simple(cExp)) {
  override def toString(e: Node_[_]): Option[String] = Some(s"(${e.kids(0)})")
}
case object TupleExp extends Exp(_ => TupleExpSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"(${e.kids.seq.mkString(", ")})")
}
case object LExp extends Exp(_ => LExpSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"[${List(e.kids).mkString(", ")}]")
}

case object ASeqExp extends Exp(sequence(simple(cExp), option(simple(cOpKind)), option(simple(cOpKind)))) {
  override def toString(e: Node_[_]): Option[String] = {
    val e0 = e.kids(0)
    if (e.kids.seq.size == 1)
      Some(s"[$e0 .. ]")
    else if (e.kids.seq.size == 2) {
      e.kids(1).kind match {
        case NNone => Some(s"[$e0 .. ]")
        case NSome => Some(s"[$e0, ${e.kids(2)} .. ]")
      }
    }
    else {
      e.kids(2).kind match {
        case NNone => e.kids(1).typ match {
          case NNone => Some(s"[$e0 .. ]")
          case NSome => Some(s"[$e0, ${e.kids(1)} .. ]")
        }
        case NSome => e.kids(1).typ match {
          case NNone => Some(s"[$e0 .. ${e.kids(2)}]")
          case NSome => Some(s"[$e0, ${e.kids(1)} .. ${e.kids(2)}]")
        }
      }
    }
  }
}

import incremental.haskell.Qual._
case object ListComp extends Exp(_ => ListCompSyntax)
object ListCompSyntax extends SyntaxChecking.SyntaxChecker(ListComp) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (kids.seq.size <= 1)
      error(s"Expected at least two kid but got one or none!")
    if (!(kids(0).kind.isInstanceOf[Exp]))
      error(s"Expected first kid an expression but got ${kids(0).kind}")
    if (kids.drop(1).exists(!_.kind.isInstanceOf[Qual]))
      error(s"All kids from second must be of sort Qual, but found ${kids.drop(1).filter(!_.kind.isInstanceOf[Qual])}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}


case object LeftSel

case object RightSel

case object LabCon extends Exp(_ => LabConSyntax) {
  override def toString(e: Node_[_]): Option[String] = {
    val con = e.kids(0)
    val vars = e.lits(0).asInstanceOf[Seq[Symbol]]
    val exps: Seq[Node_[_]] = e.kids.seq
    val lab = vars.map( sym => exps.map(ex => s"${sym.name} = ${ex.kids(0)}"))
    Some(s"$con { ${lab.mkString(", ")} }")
  }
}
case object  LabUp extends Exp(_ => LabUpSyntax) {
  override def toString(e: Node_[_]): Option[String] = {
    val aexp = e.kids(0)
    val vars = e.lits(0).asInstanceOf[Seq[Symbol]]
    val exps: Seq[Node_[_]] = e.kids.seq
    val lab = vars.map( sym => exps.map(ex => s"${sym.name} = ${ex.kids(0)}"))
    Some(s"$aexp { ${lab.mkString(", ")} }")
  }
}

import incremental.haskell.Apat._
case object Abs extends Exp(_ => AbsSyntax){
  override def toString(e: Node_[_]): Option[String] = Some(s"\ ${e.kids.seq.dropRight(1).mkString(", ")} -> ${e.kids.seq.last}")
}
object AbsSyntax extends SyntaxChecking.SyntaxChecker(Abs) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (kids.seq.dropRight(1).exists(!_.kind.isInstanceOf[Apat]))
      error(s"All kids must be of sort apat, but found ${kids.filter(!_.kind.isInstanceOf[Apat])}")
    if (!kids.last.isInstanceOf[Exp]) {

    }
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}
//case object Let extends Exp((simple(Seq(classOf[Symbol]), cExp, cExp) orElse simple(Seq(classOf[Symbol], classOf[PolType]), cExp, cExp))){ // Exp (_ => LetSyntax)
case object Let extends Exp(_ => LetSyntax)

case object App  extends Exp(simple(cExp, cExp))
case object LetPoly extends Exp(simple(Seq(classOf[Symbol]), cExp, cExp)) // TODO prove, just to see how it works

case object If  extends Exp(simple(cExp, cExp, cExp)) {
  override def toString(e:Node_[_]): Option[String] = Some(s"if ${e.kids(0)}; then ${e.kids(1)}; else ${e.kids(2)}")
}
case object Case extends Exp(_ => CaseSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"case ${e.kids(0)} of { ${e.kids.seq.drop(1).mkString(", ")} }")
}
case object Do extends Exp(_ => DoSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"do { ${e.kids.seq.dropRight(1).mkString("; ")} ${e.kids.seq.last} }")
}
case object FApp extends Exp(sequence(option(simple(cExp)), simple(cExp)))


object LabConSyntax extends SyntaxChecking.SyntaxChecker(LabCon) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (!((kids.size - 1)  == lits.size))
    error(s"Number of variables does not match the number of expressions. ${kids.size -1} != ${lits.size}")
    if (kids.exists(!_.kind.isInstanceOf[Exp]))
      error(s"All kids must be of sort Exp, but found ${kids.filter(!_.kind.isInstanceOf[Exp])}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}

}
object LabUpSyntax extends SyntaxChecking.SyntaxChecker(LabUp) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (!((kids.size - 1)  == lits.size))
      error(s"Number of variables does not match the number of expressions. ${kids.size -1} != ${lits.size}")
    if (kids.exists(!_.kind.isInstanceOf[Exp]))
      error(s"All kids must be of sort Exp, but found ${kids.filter(!_.kind.isInstanceOf[Exp])}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}

}
object FAppSyntax extends SyntaxChecking.SyntaxChecker(FApp){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (!(lits(0).isInstanceOf[Symbol]))
      error(s"Expected function name but got ${lits(0)}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}

}


object InfixSyntax extends SyntaxChecking.SyntaxChecker(Infix) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}

}

object GConBSyntax extends SyntaxChecking.SyntaxChecker(GConB) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
      if (lits.size != 1)
        error(s"Wrong number of literals. Expected one literal, got $lits")
      if (!lits(0).isInstanceOf[TCon]) //TODO Should be Data Type, instead of PolType
        error(s"Constructor name should be a Symbol, but found ${(!(lits(0).isInstanceOf[TCon]))}")
      if (kids.exists(!_.kind.isInstanceOf[Exp]))
       error(s"All kids must be of sort Exp, but found ${kids.filter(!_.kind.isInstanceOf[Exp])}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}

}
object GConSSyntax extends SyntaxChecking.SyntaxChecker(GConS) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (lits.size != 1)
      error(s"Wrong number of literals. Expected one literal, got $lits")
    if (!lits(0).isInstanceOf[TCon])
      error(s"Constructor name should be a Symbol, but found ${(!(lits(0).isInstanceOf[TCon]))}")
    if (kids.exists(!_.kind.isInstanceOf[Exp]))
      error(s"All kids must be of sort Exp, but found ${kids.filter(!_.kind.isInstanceOf[Exp])}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}

}
object GConCSyntax extends SyntaxChecking.SyntaxChecker(GConC) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (lits.size != 1)
      error(s"Wrong number of literals. Expected one literal, got $lits")
    if (!lits(0).isInstanceOf[TCon])
      error(s"Constructor name should be a Symbol, but found ${(!(lits(0).isInstanceOf[TCon]))}")
    if (kids.exists(!_.kind.isInstanceOf[Exp]))
      error(s"All kids must be of sort Exp, but found ${kids.filter(!_.kind.isInstanceOf[Exp])}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}

}
object TupleExpSyntax extends SyntaxChecking.SyntaxChecker(TupleExp) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (kids.exists(!_.kind.isInstanceOf[Exp]))
      error(s"All kids must be of sort Exp, but found ${kids.filter(!_.kind.isInstanceOf[Exp])}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}
object LExpSyntax extends SyntaxChecking.SyntaxChecker(LExp) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (kids.exists(!_.kind.isInstanceOf[Exp]))
      error(s"All kids must be of sort Exp, but found ${kids.filter(!_.kind.isInstanceOf[Exp])}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}

object LetSyntax extends SyntaxChecking.SyntaxChecker(Let) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (!(kids.seq.last.kind.isInstanceOf[Exp]))
      error(s"Expected the IN expr of sort expression but got ${kids.seq.last.kind}")
    if (kids.dropRight(1).exists(!_.kind.isInstanceOf[Decl]))
      error(s"All kids must be of sort Decl, but found ${kids.dropRight(1).filter(!_.kind.isInstanceOf[Decl])}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}

import incremental.haskell.Alternative._
object CaseSyntax extends SyntaxChecking.SyntaxChecker(Case) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (!(kids(0).kind.isInstanceOf[Exp]))
      error(s"Expected first kid an expression but got ${kids(0).kind}")
    if (kids.seq.size == 1)
      error(s"Case expresion should have at elast one alternative but found None")
    if (kids.drop(1).exists(!_.kind.isInstanceOf[Alternative]))
      error(s"All kids must be of sort Alt, but found ${kids.drop(1).filter(!_.kind.isInstanceOf[Alternative])}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}

object DoSyntax extends SyntaxChecking.SyntaxChecker(Do) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (!(kids.seq.last.kind.isInstanceOf[Exp]))
      error(s"Expected the IN expr of sort expression but got ${kids.seq.last.kind}")
    if (kids.dropRight(1).exists(!_.kind.isInstanceOf[Stmt]))
      error(s"All kids must be of sort Stmt, but found ${kids.dropRight(1).filter(!_.kind.isInstanceOf[Stmt])}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}

//object ThisSyntax extends SyntaxChecking.SyntaxChecker(Let) {
//  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
//  }
//}