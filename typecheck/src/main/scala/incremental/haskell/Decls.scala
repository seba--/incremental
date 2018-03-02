package incremental.haskell

import incremental.haskell.{NodeKind, SyntaxChecking}
import incremental.haskell.Node._
import incremental.haskell.SyntaxChecking.SyntaxCheck
import incremental.haskell._
import incremental.haskell.{Node_, SyntaxChecking}

import scala.collection.immutable.ListMap
import incremental.haskell.Literal._
import incremental.haskell.OptionKind.option

abstract class Toplevel(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Toplevel {
  val cTlevel = classOf[Toplevel]
}

case object simpletype extends NodeKind(simple())
case object context extends NodeKind(simple())
case object constrs extends NodeKind(simple())
case object deriving extends NodeKind(simple())
case object scontext extends NodeKind(simple())
case object newconstrs extends NodeKind(simple())
case object tycls extends NodeKind(simple())
case object tyvar extends NodeKind(simple())
case object qtycls extends NodeKind(simple())
case object idecls extends Toplevel(simple())



case object TypeDecl extends NodeKind(_ => TypeDecSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"type ${e.lits(0)} = ${e.lits(1)}")
}

case object DataDecl extends NodeKind(_ => DataDeclSyntax) {
  override def toString(e: Node_[_]): Option[String] = {
    if (e.kids.seq.size == 1)
      Some(s"data ${e.kids(0)}")
    else if (e.kids.seq.size == 2){
      if (e.kids(0).kind == context)
        Some(s"data ${e.kids(0)} => ${e.kids(1)}")
      else if (e.kids(1).kind == constrs)
        Some(s"data ${e.kids(0)} = ${e.kids(1)}")
      else Some(s"data ${e.kids(0)} ${e.kids(1)}")
    }
    else if (e.kids.seq.size == 3) {
      if (e.kids(0).kind == context && e.kids(2).kind == constrs)
        Some(s"data ${e.kids(0)} => ${e.kids(1)} = ${e.kids(2)}")
      else if (e.kids(0).kind == context && e.kids(2).kind == deriving)
        Some(s"data ${e.kids(0)} => ${e.kids(1)} ${e.kids(2)}")
      else Some(s"data ${e.kids(0)} = ${e.kids(1)} ${e.kids(2)}")
    }
    else Some(s"data ${e.kids(0)} => ${e.kids(1)} = ${e.kids(2)} ${e.kids(3)}")
  }
}
case object newtype extends NodeKind(_ => newtypeSyntax) {
  override def toString(e: Node_[_]): Option[String] = {
    if (e.kids.seq.size == 2)
      Some(s"newtype ${e.kids(0)} = ${e.kids(1)}")
    else if (e.kids.seq.size == 3){
      if ()
    }
    )
  }
}


case object ProgramM extends NodeKind(_ => ProgramSyntax) {
  override def toString(e: Node_[_]): Option[String] = {
    Some(e.kids.seq.mkString("{", "\n", "}"))
  }
}

case object ClassDec extends Toplevel(_ => ClassSyntax) {
  override def toString(e: Node_[_]): Option[String] = {
    val name = e.lits(0).asInstanceOf[Symbol]
    val typVar = e.lits(1).asInstanceOf[Seq[PolType]]
    val sup = e.lits(2).asInstanceOf[Seq[(Symbol, PolType)]]
    val indent = "  "
    val vars = e.lits(3).asInstanceOf[Seq[(Symbol, PolType)]].map { case (vars, typ) => s"val ${vars.name}: $typ" }

    val functions: Seq[Node_[_]] = e.kids.seq

    Some(s"class $sup => $name {\n${vars.mkString(indent, "\n"+indent, "\n")}\n$indent${functions.mkString(indent, "\n"+indent, "\n")}\n}")
  }
}

//TODO see again instance decl
case object InstDec extends Toplevel(_ => InstSyntax) {
  override def toString(e: Node_[_]): Option[String] = {
    val name = e.lits(0)
    val typ = e.lits(1)
    Some(s"instance ${e.lits(0).asInstanceOf[Symbol].name} ${e.lits(1)} where" +
            s" ${e.kids.seq}")
  }
}
case object FuncDec extends Toplevel(_ => FuncSyntax) {
  override def toString(e: Node_[_]): Option[String] = {
    val name = e.lits(0).asInstanceOf[Symbol].name
    val params = e.lits(1).asInstanceOf[Seq[PolType]].map { case (typ) => s"$typ" }
    val ret = e.lits(2).asInstanceOf[PolType]
    Some(s"$name :: ${params.mkString("")}) -> $ret" )
  }
}



object TypeDecSyntax extends SyntaxChecking.SyntaxChecker(TypeDecl) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
    if (!(lits(0).isInstanceOf[PolType]))
      error(s"Expected Type, but got ${lits(0)}")
    if (!(kids(0).kind == simpletype))
      error(s"Expected simpletype but got ${kids(0).kind}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}
object DataDeclSyntax extends SyntaxChecking.SyntaxChecker(DataDecl){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {}
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {
    if (kids.size == 1 && kids(0) == simpletype )
    error(s"Expected simpletype but got ${kids(0)}")
  }
}


object ClassSyntax extends SyntaxChecking.SyntaxChecker(ClassDec) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]){
    if (!(lits(0).isInstanceOf[Symbol]))
      error(s"Expected Class Name, but got ${lits(0)}")
    if (kids.exists(k => !(k.kind == FuncDec)))
      error(s"Expected function declarations but got ${kids.filter(k => !(k.kind == FuncDec))}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}

object InstSyntax extends SyntaxChecking.SyntaxChecker(InstDec) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]){
    if (!(lits(0).isInstanceOf[Symbol]))
      error(s"Expected Instance Name, but got ${lits(0)}")
    if (kids.nonEmpty)
      error(s"Expected no kids but got $kids")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}

object FuncSyntax extends SyntaxChecking.SyntaxChecker(FuncDec) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]){
    if (!(lits(0).isInstanceOf[Symbol]))
      error(s"Expected Function Name, but got ${lits(0)}")
    if (!(lits(1).isInstanceOf[PolType]))
      error(s"Expected Function Type, but got ${lits(1)}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}

object ProgramSyntax extends SyntaxChecking.SyntaxChecker(ProgramM) {
  def check[T](lits: Seq[Lit] , kids: Seq[Node_[T]]){
    if (lits.nonEmpty)
      error(s"No literals expected")
    kids.foreach( k =>
      assert(k.kind == ClassDec || k.kind == ProgramM)
    )
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}
