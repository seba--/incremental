package incremental.haskell

import incremental.haskell.{NodeKind, SyntaxChecking}
import incremental.haskell.Node._
import incremental.haskell.SyntaxChecking.SyntaxCheck
import incremental.haskell._
import incremental.haskell.{Node_, SyntaxChecking}

import scala.collection.immutable.ListMap
import incremental.haskell.Literal._
import incremental.haskell.OptionKind.option
import incremental.haskell.Pat._
import incremental.haskell.Exp._
import incremental.haskell.OptionKind.option


abstract class Toplevel(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Toplevel {
  val cTlevel = classOf[Toplevel]
}

case object constrs extends NodeKind(simple())
case object deriving extends NodeKind(simple())
case object scontext extends NodeKind(simple())
case object newconstrs extends NodeKind(simple())
case object tycls extends NodeKind(simple())
case object tyvar extends NodeKind(simple())
case object qtycls extends NodeKind(simple())
case object idecls extends Toplevel(simple())

import incremental.haskell.AType._
import incremental.haskell.Gtycon._
import incremental.haskell.TypeH._

//===========================Ops======================
import incremental.haskell.Op._
import incremental.haskell.Conop._
import incremental.haskell.Varop._
import incremental.haskell.Btype._

abstract class Op extends NodeKind(simple(Seq(conop)) orElse simple(Seq(varop)))
object Op {
  val op = classOf[Op]
}

abstract class Conop extends NodeKind(simple(Seq(classOf[Symbol])))
object Conop {
  val conop = classOf[Conop]

}

abstract class Varop extends NodeKind(simple(Seq(classOf[Symbol])))
object Varop {
  val varop = classOf[Varop]
}

 //=====================TYPES ==========================================
case object TypeDecl extends NodeKind(_ => TypeDecSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"type ${e.lits(0)} = ${e.lits(1)}")
}

abstract class TypeH(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object TypeH {
  val typeh = classOf[TypeH]
}
case object FunType extends TypeH(_ => TypeSyntax) {
  override def toString(e: Node_[_]): Option[String] = {
    if (e.lits.size == 1)
      Some(s"${e.lits(0)}")
    else Some(s"${e.lits(0)} -> ${e.lits(1)}")
  }
}

abstract class Btype extends TypeH(simple(Seq(btype, atype)) orElse simple(Seq(atype)))
//{
//  override def toString(e: Node_[_]): Option[String] = {
//    if (e.lits.size == 1)
//      Some(s"${e.lits(0)}")
//    else Some(s"${e.lits(0)} ${e.lits(1)}")
//  }
//}
object Btype {
  val btype = classOf[Btype]
}


abstract class AType(syntaxcheck: SyntaxChecking.SyntaxCheck) extends TypeH(syntaxcheck)
object AType {
  val atype = classOf[AType]
}

case object tyvar extends AType(simple(Seq(classOf[Symbol])))

case object tupleType extends AType(_ => tupleTypeSyntax) {
  override def toString(e: Node_[_]): Option[String] =
    Some(s"( ${e.lits.mkString(", ")} )")
}

case object listType extends AType(simple(typeh)) {
  override def toString(e: Node_[_]): Option[String] =
    Some(s"[ ${e.lits(0)} ]")
}

case object paType extends AType(simple(typeh)) {
  override def toString(e: Node_[_]): Option[String] =
    Some(s"( ${e.lits(0)} )")
}

abstract class Gtycon(syntaxcheck: SyntaxChecking.SyntaxCheck) extends AType(syntaxcheck)
object Gtycon {
  val gtycon = classOf[Gtycon]
}

case object unitType extends Gtycon(simple()) {
  override def toString(e: Node_[_]): Option[String] =
    Some(s"( )")
}
case object listCon extends Gtycon(simple()) {
  override def toString(e: Node_[_]): Option[String] =
    Some(s"[ ]")
}
case object funCon extends Gtycon(simple()) {
  override def toString(e: Node_[_]): Option[String] =
    Some(s"( -> )")
}
case object tupCon extends Gtycon(simple()) {
  override def toString(e: Node_[_]): Option[String] =
    Some(s"( ,{,} )")
}

abstract class Qtycon(syntaxcheck: SyntaxChecking.SyntaxCheck) extends Gtycon(syntaxcheck)
object Qtycon {
  val qtycon = classOf[Qtycon]
}

object TypeDecSyntax extends SyntaxChecking.SyntaxChecker(TypeDecl) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
    if (!(lits(0) == simpletype))
      error(s"Expected simpletype, but got ${lits(0)}")
    if (!(lits(1) == TypeH))
      error(s"Expected simpletype but got ${lits(1)}")
  }

  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}

object TypeSyntax extends SyntaxChecking.SyntaxChecker(FunType) {
  override def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
    if (lits.size == 1 && !(lits(0).asInstanceOf[Btype]))
      error(s"Expectd a type application but got ${lits(0)}")
    if (lits.size == 2 && !(lits(1).isInstanceOf[PolType]))
      error(s"Expectd a polymorphic but got ${lits(1)}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}

//object BTypeSyntax extends SyntaxChecking.SyntaxChecker(Btype) {
//  override def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
//    if (lits.size == 1 && lits(0).isInstanceOf[AType])
//      error(s"Expected type apllication but got ${lits(0)}")
//  }
//  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
//}

object tupleTypeSyntax extends SyntaxChecking.SyntaxChecker(tupleType) {
  override def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
    if (lits.size < 2 )
      error(s"Expected at least 2 Types but got ${lits.size}")
    if (lits.exists(!_.isInstanceOf[TypeH]))
      error(s"All lits must be of sort Type, but found ${lits.filter(!_.isInstanceOf[TypeH])}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}

// ======================================================================

case object context extends NodeKind(_ => ContxSyntax)

object ContxSyntax extends SyntaxChecking.SyntaxChecker(context) {
  override def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (lits.exists(!_.equals(Class)))
      error(s"All lits must be of sort  Class, but found ${lits.filter(!_.equals(Class))}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}

}
case object Class extends NodeKind(simple(Seq(classOf[Symbol], classOf[Symbol], classOf[Seq[AType]]))) {
  override def toString(e: Node_[_]): Option[String] = {
    if (e.lits.size == 2 )
      Some(s"${e.lits(0)} ${e.lits(1)}")
    else Some(s"${e.lits(0)} ( ${e.lits(1)} ${e.lits.drop(2).mkString(" ")} )")
  }
}

case object scontext extends NodeKind(_ => SContxSyntax)

object SContxSyntax extends SyntaxChecking.SyntaxChecker(scontext) {
  override def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (lits.exists(!_.equals(SClass)))
      error(s"All lits must be of sort  SimpleClass, but found ${lits.filter(!_.equals(SClass))}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}

}
case object SClass extends NodeKind(simple(Seq(classOf[Symbol], classOf[Symbol]))) {
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.lits(0)} ${e.lits(1)}")
}

case object simpletype extends NodeKind(simple(Seq(classOf[Symbol], classOf[Seq[Symbol]])))

case object cons extends NodeKind(simple(Seq(classOf[Symbol], classOf[Seq[AType]])))

case object infixcon extends NodeKind(simple(Seq(btype, conop, btype)) orElse simple(Seq(atype, conop, atype)))

case object fieldcon extends NodeKind(_ => fieldconSyntax){
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.lits(0)} { ${e.lits.drop(1).mkString(", ")} }")
}
object fieldconSyntax  extends SyntaxChecking.SyntaxChecker(fieldcon) {
  override def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (lits.size == 1 && !(lits(0).isInstanceOf[Symbol]))
      error(s"Constructor name expected a Symbol but got ${lits(0)}")
    if (lits.drop(1).exists(!_.equals(fielddecl)))
      error(s"All lits must be of sort Field Dec, but found ${lits.drop(1).filter(!_.equals(fielddecl))}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}
case object newconstr extends NodeKind(simple())

case object fielddecl extends Toplevel(_ => fielddeclSyntax) { // TODO see this again, how to express the syntax
  override def toString(e: Node_[_]): Option[String] = {
    if (e.lits.size == 2)
      Some(s"${e.lits(0)} :: ${e.lits(1)}")
    else
      Some(s"${e.lits.dropRight(1).mkString(" ,")} :: ${e.lits.last}")
  }
}
object fielddeclSyntax  extends SyntaxChecking.SyntaxChecker(fielddecl){
  override def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (lits.dropRight(1).exists(!_.isInstanceOf[Symbol]))
      error(s"All Field names must be of sort Symbol, but found ${lits.drop(1).filter(!_.isInstanceOf[Symbol])}")
    if ((!lits.last.isInstanceOf[TypeH]))
      error(s"Fields Type expected of sort TypeH, but found ${lits.last}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}

abstract class Inst(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Inst {
  val inst = classOf[Inst]
}

case object InstTyVars extends Inst(simple(Seq(gtycon, classOf[Seq[Symbol]]))){
  override def toString(e: Node_[_]): Option[String] = Some(s"( ${e.lits.mkString(" ")} )")
}
case object InstTyVarsTup extends Inst(simple(Seq(classOf[Seq[Symbol]]))){
  override def toString(e: Node_[_]): Option[String] = Some(s"( ${e.lits.mkString(", ")} )")
}
case object InstTyVar extends Inst(simple(Seq(gtycon, classOf[Symbol]))){
  override def toString(e: Node_[_]): Option[String] = Some(s"[ ${e.lits(0)} ]")
}
case object InstTyVar2 extends Inst(simple(Seq(classOf[Symbol], classOf[Symbol]))){
  override def toString(e: Node_[_]): Option[String] = Some(s"( ${e.lits(0)} -> ${e.lits(1)} )")
}



//=================================================================================================

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
      if (e.kids(0).kind == context)
        Some(s"newtype ${e.kids(0)} => ${e.kids(1)} = ${e.kids(2)}")
      else
        Some(s"newtype ${e.kids(0)} = ${e.kids(1)} ${e.kids(2)}")
    }
    else
      Some(s"newtype ${e.kids(0)} => ${e.kids(1)} = ${e.kids(2)} ${e.kids(3)}")
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

//=================-------------------------------------
import incremental.haskell.Rhs._
import incremental.haskell.Funlhs

abstract class Declaration(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Declaration {
  val decl = classOf[Declaration]
}

case object Decl extends Declaration(simple(funlhs, rhs) orElse simple(cPat, rhs))

case object TypeSign extends Declaration(_ => TypeSignSyntax) {
  override def toString(e: Node_[_]): Option[String] = {

  }
}
//=============================================

abstract class Rhs(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Rhs {
  val rhs = classOf[Rhs]
}

abstract class Funlhs(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Funlhs {
  val funlhs = classOf[Funlhs]
}


object DataDeclSyntax extends SyntaxChecking.SyntaxChecker(DataDecl){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {}
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {
    if (kids.size == 1 && kids(0) == simpletype )
    error(s"Expected simpletype but got ${kids(0)}")
  }
}

object newtypeSyntax extends SyntaxChecking.SyntaxChecker(newtype){
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
