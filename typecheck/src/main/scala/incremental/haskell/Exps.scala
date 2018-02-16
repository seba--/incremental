package incremental.haskell

import incremental.{NodeKind, SyntaxChecking}
import incremental.Node._
import incremental.{Node_, SyntaxChecking}

import scala.collection.immutable.ListMap


abstract class Toplevel(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)

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

abstract class Exp(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Exp {
  val cExp = classOf[Exp]
}
import Exp._

case object Var extends Exp(simple(Seq(classOf[Symbol]))) {
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.lits(0).asInstanceOf[Symbol].name}")
}
case object GConB extends Exp(_ => GConBSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.lits(0)}(${e.kids.seq.mkString(", ")})")
}
case object GConS extends Exp(_=> GConSSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.lits(0)}[${e.kids.seq.mkString(", ")}]")
}
case object GConC extends Exp(_=> GConCSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.lits(0)}{${e.kids.seq.mkString(", ")}}")
}
case object Lit extends Exp(simple(cExp))
case object PExp extends Exp(simple(cExp)) {
  override def toString(e: Node_[_]): Option[String] = Some(s"(${e.kids(0)})")
}
case object PSExp extends Exp(simple(Seq(cExp))) {
  override def toString(e: Node_[_]): Option[String] = Some(s"(${e.kids.seq.mkString(", ")})")
}
case object LExp extends Exp(simple(List(cExp))) {
  override def toString(e: Node_[_]): Option[String] = Some(s"[${List(e.kids).mkString(", ")}]")
}
case object AExp extends Exp(simple(List(cExp))) {
  override def toString(e: Node_[_]): Option[String] = Some(s"[${e.kids(0)} [, ${e.kids(1)}] .. [${e.kids(2)}]]")
}
case object ListCom

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
case object  LabExp extends Exp(_ => LabExpSyntax) {
  override def toString(e: Node_[_]): Option[String] = {
    val aexp = e.kids(0)
    val vars = e.lits(0).asInstanceOf[Seq[Symbol]]
    val exps: Seq[Node_[_]] = e.kids.seq
    val lab = vars.map( sym => exps.map(ex => s"${sym.name} = ${ex.kids(0)}"))
    Some(s"$aexp { ${lab.mkString(", ")} }")
  }
}
case object Abs  extends Exp(simple(Seq(classOf[Symbol]), cExp) orElse simple(Seq(classOf[Symbol], classOf[PolType]), cExp) orElse (simple(Seq(classOf[Seq[Symbol]]), cExp)))
case object Let extends Exp((simple(Seq(classOf[Symbol]), cExp, cExp) orElse simple(Seq(classOf[Symbol], classOf[PolType]), cExp, cExp))){ // Exp (_ => LetSyntax)
  override def toString(e: Node_[_]): Option[String] = {
    if (e.lits.size == 1) {
      val sym =  e.lits(0).asInstanceOf[Symbol]
      val typ = e.lits(1).asInstanceOf[PolType]
      Some(s"Let $sym: $typ  = ${e.kids(0)} in ${e.kids(1)}")
    }
    else{
      val sym =  e.lits(0).asInstanceOf[Symbol]
      Some(s"Let $sym  = ${e.kids(0)} in ${e.kids(1)}")
    }
  }
}
case object If  extends Exp(simple(cExp, cExp, cExp)) {
  override def toString(e:Node_[_]): Option[String] = Some(s"if ${e.kids(0)}; then ${e.kids(1)}; else ${e.kids(2)}")
}
case object Case extends Exp(simple(Seq(cExp))) { //TODO see tis again
  override def toString(e: Node_[_]): Option[String] = Some(s"case ${e.kids(0)}")
}
case object Do extends Exp(simple(Seq(cExp))) { // TODO see this again
  override def toString(e: Node_[_]): Option[String] = Some(s"do { ${e.kids.seq.mkString(", ")} }")
}
case object FApp extends Exp(simple(cExp, cExp))

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
case object NLit extends Exp(_ => NlitSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"-(${e.kids.seq.mkString(", ")})")
}
case object PatCon extends Pat(_ => PatConSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.kids.seq.mkString(" ")}")
}
case object VarP extends Pat(simple(Seq(classOf[Symbol]), cPat)) {
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.lits(0)} [ @${e.kids(0)} ]")
}
case object ZCon extends Pat(_ => ZConSyntax)
case object LabPat extends Pat(_ => LPatSyntax){
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.kids(0)} { ${e.kids.seq.mkString(", ")} }")
}
case object LitPat extends Pat(simple(cExp))

case object WildCPat extends Pat(simple(cExp)) // TODO see this again

case object PPat extends Pat(simple(cPat)) {
  override def toString(e: Node_[_]): Option[String] = Some(s"( ${e.kids(0)} )")
}
case object TupPat extends Pat(_ => TupPatSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"(  )")
}


object PatMSyntax extends SyntaxChecking.SyntaxChecker(PatM) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
     if (!(kids(1).kind == GConB) || !(kids(1).kind == GConS) || !(kids(1).kind == GConC) )
       error(s"Expected Constructor but got ${kids(1)}")
    if (!(kids(1).kids.seq.size == kids.size -2))
       error(s"The arity of a constructor does not match the number of sub-patterns associated with it")
  }
}
object NlitSyntax extends SyntaxChecking.SyntaxChecker(PatM) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (kids.exists(k => !(k.kind == Lit)))
      error(s"Expected Literal but got ${kids.filter(k => !(k.kind == Lit))}")
  }
}
object PatConSyntax extends SyntaxChecking.SyntaxChecker(PatCon) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (!(kids(0).kind == GConB) || !(kids(0).kind == GConS) || !(kids(0).kind == GConC))
      error(s"Expected Constructor but got ${kids(0).kind}")
    if (!(kids(0).kids.seq.size == kids.size - 1))
      error(s"The arity of a constructor does not match the number of sub-patterns associated with it")
  }
}
object ZConSyntax extends SyntaxChecking.SyntaxChecker(ZCon) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (!(lits(0).isInstanceOf[Symbol]))
      error(s"Constructor name expected a Symbol but got ${lits(0)}")
    if (kids.nonEmpty)
      error(s"Expected no kids but got $kids")
  }
}
object LPatSyntax extends SyntaxChecking.SyntaxChecker(LabPat) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (!(kids(0).kind == GConB) || !(kids(0).kind == GConS) || !(kids(0).kind == GConC))
      error(s"Expected Constructor but got ${kids(0).kind}")
    if (!(kids(0).kids.seq.size == kids.size - 1))
      error(s"The arity of a constructor does not match the number of sub-patterns associated with it")
  }
}
object TupPatSyntax extends SyntaxChecking.SyntaxChecker(LabPat) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (!(kids.size >= 2))
      error(s"Expected tuple with at least 2 elements but got only ${kids.size}")
  }
}


object LabConSyntax extends SyntaxChecking.SyntaxChecker(LabCon) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (!((kids.size - 1)  == lits.size))
    error(s"Number of variables does not match the number of expressions. ${kids.size -1} != ${lits.size}")
    if (kids.exists(!_.kind.isInstanceOf[Exp]))
      error(s"All kids must be of sort Exp, but found ${kids.filter(!_.kind.isInstanceOf[Exp])}")
  }
}
object LabExpSyntax extends SyntaxChecking.SyntaxChecker(LabExp) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (!((kids.size - 1)  == lits.size))
      error(s"Number of variables does not match the number of expressions. ${kids.size -1} != ${lits.size}")
    if (kids.exists(!_.kind.isInstanceOf[Exp]))
      error(s"All kids must be of sort Exp, but found ${kids.filter(!_.kind.isInstanceOf[Exp])}")
  }
}

case object Infix extends Exp(_ => InfixSyntax) {
  override def toString(e: Node_[_]): Option[String] = {
    val lexp = e.kids(0)
    val op = e.lits(0).asInstanceOf[Symbol]//TODO See this again expr1 '+ expr2
    val infixexp = e.kids(1)
    Some(s"$lexp $op $infixexp")
  }
}


case object Num  extends Exp(simple(Seq(classOf[Integer])))
case object CFloat  extends Exp(simple(Seq(classOf[Integer])))
case object CDouble  extends Exp(simple(Seq(classOf[Integer])))
case object CInt  extends Exp(simple(Seq(classOf[Integer])))
case object CChar  extends Exp(simple(Seq(classOf[Symbol])))

case object TAdd  extends Exp(simple(cExp, cExp))
case object TMul  extends Exp(simple(cExp, cExp))
case object Add  extends Exp(simple(cExp, cExp))
case object Mul  extends Exp(simple(cExp, cExp))
//case object Abs  extends Exp(simple(Seq(classOf[Symbol]), cExp) orElse simple(Seq(classOf[Symbol], classOf[PolType]), cExp) orElse (simple(Seq(classOf[Seq[Symbol]]), cExp)))
case object TAbs extends Exp(simple(Seq(classOf[Symbol]), cExp))
case object App  extends Exp(simple(cExp, cExp))
case object TApp extends Exp(simple(Seq(classOf[PolType]), cExp))
case object Fix  extends Exp(simple(cExp))
//case object Let extends Exp(simple(Seq(classOf[Symbol]), cExp, cExp))
//case object Inst extends Exp(simple(Seq(classOf[Symbol], classOf[PolType]), cExp, cExp))


object InfixSyntax extends SyntaxChecking.SyntaxChecker(Infix) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {

  }
}

object GConBSyntax extends SyntaxChecking.SyntaxChecker(GConB) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
      if (lits.size != 1)
        error(s"Wrong number of literals. Expected one literal, got $lits")
      if (!lits(0).isInstanceOf[Symbol])
        error(s"Constructor name should be a Symbol, but found ${(!(lits(0).isInstanceOf[Symbol]))}")
//      if (kids.exists(!_.kind.isInstanceOf[Exp]))
//        error(s"All kids must be of sort Exp, but found ${kids.filter(!_.kind.isInstanceOf[Exp])}")

  }
}
object GConSSyntax extends SyntaxChecking.SyntaxChecker(GConS) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (lits.size != 1)
      error(s"Wrong number of literals. Expected one literal, got $lits")
    if (!lits(0).isInstanceOf[Symbol])
      error(s"Constructor name should be a Symbol, but found ${(!(lits(0).isInstanceOf[Symbol]))}")
    //      if (kids.exists(!_.kind.isInstanceOf[Exp]))
    //        error(s"All kids must be of sort Exp, but found ${kids.filter(!_.kind.isInstanceOf[Exp])}")

  }
}
object GConCSyntax extends SyntaxChecking.SyntaxChecker(GConC) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (lits.size != 1)
      error(s"Wrong number of literals. Expected one literal, got $lits")
    if (!lits(0).isInstanceOf[Symbol])
      error(s"Constructor name should be a Symbol, but found ${(!(lits(0).isInstanceOf[Symbol]))}")
    //      if (kids.exists(!_.kind.isInstanceOf[Exp]))
    //        error(s"All kids must be of sort Exp, but found ${kids.filter(!_.kind.isInstanceOf[Exp])}")

  }
}



object ClassSyntax extends SyntaxChecking.SyntaxChecker(ClassDec) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]){

    if (!(lits(0).isInstanceOf[Symbol]))
      error(s"Expected Class Name, but got ${lits(0)}")

    if (kids.exists(k => !(k.kind == FuncDec)))
      error(s"Expected function declarations but got ${kids.filter(k => !(k.kind == FuncDec))}")
  }
}

object InstSyntax extends SyntaxChecking.SyntaxChecker(InstDec) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]){

    if (!(lits(0).isInstanceOf[Symbol]))
      error(s"Expected Instance Name, but got ${lits(0)}")

    if (kids.nonEmpty)
      error(s"Expected no kids but got $kids")
  }
}

object FuncSyntax extends SyntaxChecking.SyntaxChecker(FuncDec) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]){

    if (!(lits(0).isInstanceOf[Symbol]))
      error(s"Expected Function Name, but got ${lits(0)}")

    if (!(lits(1).isInstanceOf[PolType]))
      error(s"Expected Function Type, but got ${lits(1)}")
  }
}

object ProgramSyntax extends SyntaxChecking.SyntaxChecker(ProgramM) {
  def check[T](lits: Seq[Lit] , kids: Seq[Node_[T]]){
    if (lits.nonEmpty)
      error(s"No literals expected")

    kids.foreach( k =>
      assert(k.kind == ClassDec || k.kind == ProgramM)
    )
  }
}

object ThisSyntax extends SyntaxChecking.SyntaxChecker(Let) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
  }
}