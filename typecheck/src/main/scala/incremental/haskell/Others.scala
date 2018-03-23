package incremental.haskell

import incremental.haskell.{NodeKind, SyntaxChecking}
import incremental.haskell.Node._
import incremental.haskell.SyntaxChecking.SyntaxCheck
import incremental.haskell._
import incremental.haskell.{Node_, SyntaxChecking}

import scala.collection.immutable.ListMap
import incremental.haskell.Literal._
import incremental.haskell.OptionKind.option
import incremental.haskell.Exp._
import incremental.haskell.Pat._
import incremental.haskell.DeclsA._



abstract class Alternative(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Alternative {
  val alt = classOf[Alternative]
}

case object Alt extends Alternative(_ => AlternativeSyntax) {
  override def toString(e: Node_[_]): Option[String] = {
    if (e.kids.seq == 2 )
      Some(s"${e.kids(0)} ->  ${e.kids(1)}")
    else
      Some(s"${e.kids(0)} => ${e.kids(1)} where { ${e.kids.seq.drop(2).mkString(", ")} }")
  }
}

case object EmptyAlt extends Alternative(simple())

object AlternativeSyntax extends SyntaxChecking.SyntaxChecker(Alt) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (!(kids(0).kind.isInstanceOf[Pat]))
      error(s"Expected first kid an expression but got ${kids(0).kind}")
    if (!(kids(1).kind.isInstanceOf[Exp]))
      error(s"Expected second kid an expression but got ${kids(1).kind}")
    if (kids.drop(2).exists(!_.isInstanceOf[Decl]))
      error(s"All kids of Alt must be of sort Decl, but found ${kids.seq.drop(2).filter(!_.isInstanceOf[Decl])}")

  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}

abstract class Stmt(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Statement {
  val stmt = classOf[Stmt]
}

case object ExpStmt extends Stmt(simple(cExp)){
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.kids(0)} ;")
}

case object PatStmt extends Stmt(simple(pat, cExp)){
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.kids(0)} <- ${e.kids(1)} ;")
}

case object LetStmt extends Stmt(_ => LetStmtSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"let { ${e.lits.mkString(", ")} };")
}
object LetStmtSyntax extends SyntaxChecking.SyntaxChecker(LetStmt) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (kids.exists(!_.kind.isInstanceOf[Decl]))
      error(s"All kids must be of sort Decl, but found ${kids.filter(!_.kind.isInstanceOf[Decl])}")
  }

  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}

case object EmptyStmt extends Stmt(simple()){
  override def toString(e: Node_[_]): Option[String] = Some(s" ;")
}

abstract class Bind(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Bind {
  val fbind = classOf[Bind]
}

case object FBind extends Bind(simple(Seq(classOf[Symbol]), cExp)) {
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.lits(0)} = ${e.kids(0)}")
}


abstract class Qual(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Qual {
  val qual = classOf[Qual]
}

case object Generator extends Qual(simple(pat, cExp)){
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.kids(0)} <- ${e.kids(1)}")
}
case object LocalGen extends Qual(simple(decls)) {
  override def toString(e: Node_[_]): Option[String] = Some(s"let { ${e.lits.mkString(", ")} }")
}
case object Guard extends Qual(simple(cExp))