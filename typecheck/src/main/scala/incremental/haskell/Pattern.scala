package incremental.haskell

import incremental.haskell.Node._
import incremental.haskell.Exp._
import incremental.haskell.OptionKind.option
import incremental.haskell.SyntaxChecking.SyntaxCheck

import scala.collection.immutable.ListMap


//Pattern matching
abstract class Pat(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Pat {
  val cPat = classOf[Pat]
}
import Pat._
case object PatM extends Pat(_ => PatMSyntax) {
  override def toString(e: Node_[_]): Option[String] = {
    val lpat = e.kids(0)
    val con = e.kids(1)
    val pat = e.kids.seq
    Some(s"")
  }
}
case object NLit extends Exp(_ => NLitSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"-(${e.kids.seq.mkString(", ")})")
}
case object PatCon extends Pat(_ => PatConSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.kids(0)} ${e.kids.seq.mkString(" ")}")
}
case object VarP extends Pat(sequence(simple(Seq(classOf[Symbol])), option(simple(cPat)))) {
  override def toString(e: Node_[_]): Option[String] = {
    val v = e.lits(0)
    if (e.kids.seq.size == 1)
      Some(s"$v")
    else {
      e.kids(0).kind match {
        case NNone => Some(s"$v")
        case NSome => Some(s"$v [ @${e.kids(0)} ]")
      }
    }
  }
}
case object ZCon extends Pat(_ => ZConSyntax)

case object LabPat extends Pat(_ => LPatSyntax){
  override def toString(e: Node_[_]): Option[String] = {
    val con = e.kids(0)
    val vars = e.lits
    val pats = e.kids.seq.drop(1)
    val lab = vars.map( s => pats.map( p => s"$s = $p"))
    Some(s"${e.kids(0)} { ${lab.mkString(", ")} }")
  }
}
case object LitPat extends Pat(simple(cExp))

case object WildCPat extends Pat(simple(cExp)) // TODO see this again

case object PPat extends Pat(simple(cPat)) {
  override def toString(e: Node_[_]): Option[String] = Some(s"( ${e.kids(0)} )")
}
case object TupPat extends Pat(_ => TupPatSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"( ${e.kids.seq.mkString(", ")} )")
}
case object ListPat extends Pat(_ => ListPatSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"[ ${e.kids.seq.mkString(", ")} ]")
}

object PatMSyntax extends SyntaxChecking.SyntaxChecker(PatM) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
     if (!(kids(1).kind == GConB) || !(kids(1).kind == GConS) || !(kids(1).kind == GConC) )
       error(s"Expected Constructor but got ${kids(1)}")
    if (!(kids(1).kids.seq.size == kids.size -2))
       error(s"The arity of a constructor does not match the number of sub-patterns associated with it")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}

}
object NLitSyntax extends SyntaxChecking.SyntaxChecker(NLit) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (kids.exists(k => !(k.kind == Lit)))
      error(s"Expected Literal but got ${kids.filter(k => !(k.kind == Lit))}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}
object PatConSyntax extends SyntaxChecking.SyntaxChecker(PatCon) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (!(kids(0).kind == GConB) || !(kids(0).kind == GConS) || !(kids(0).kind == GConC))
      error(s"Expected Constructor but got ${kids(0).kind}")
    if (!(kids(0).kids.seq.size == kids.size - 1))
      error(s"The arity of a constructor does not match the number of sub-patterns associated with it")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}
object ZConSyntax extends SyntaxChecking.SyntaxChecker(ZCon) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (!(lits(0).isInstanceOf[Symbol]))
      error(s"Constructor name expected a Symbol but got ${lits(0)}")
    if (kids.nonEmpty)
      error(s"Expected no kids but got $kids")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}
object LPatSyntax extends SyntaxChecking.SyntaxChecker(LabPat) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (!(kids(0).kind == GConB) || !(kids(0).kind == GConS) || !(kids(0).kind == GConC))
      error(s"Expected Constructor but got ${kids(0).kind}")
    if (!(kids(0).kids.seq.size == kids.size - 1))
      error(s"The arity of a constructor does not match the number of sub-patterns associated with it")
    if (!(kids.size - 1 == lits.size))
      error(s"Number of variables not equal to the number of paterns")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}
object TupPatSyntax extends SyntaxChecking.SyntaxChecker(LabPat) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (!(kids.size >= 2))
      error(s"Expected tuple with at least 2 elements but got only ${kids.size}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}
object ListPatSyntax extends SyntaxChecking.SyntaxChecker(ListPat) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (kids.isEmpty)
      error(s"Expected kids but got none")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}

}
