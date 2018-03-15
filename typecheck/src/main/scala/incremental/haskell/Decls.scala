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
import org.scalameter.Warmer.Default


abstract class Toplevel(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Toplevel {
  val cTlevel = classOf[Toplevel]
}

import incremental.haskell.AType._
import incremental.haskell.Gtycon._
import incremental.haskell.TypeH._

//===========================Ops======================
import incremental.haskell.Op._
import incremental.haskell.Conop._
import incremental.haskell.Varop._
import incremental.haskell.Btype._
import incremental.haskell.Ftype._

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

abstract class Fixity extends NodeKind(simple(Seq(classOf[Symbol]))) //TODO see this again(how to express infixl or right)
object Fixity {
  val fixity = classOf[Fixity]
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
    if (lits.size == 1 && !(lits(0).isInstanceOf[Btype]))
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

abstract class ScontextA(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object ScontextA {
  val scontext = classOf[ScontextA]
}
case object Scontext extends ScontextA(_ => SContxSyntax)

object SContxSyntax extends SyntaxChecking.SyntaxChecker(Scontext) {
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

abstract class Constr(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Constr {
  val constr = classOf[Constr]
}
case object constrs extends Constr(_ => constrsSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.lits.mkString(" | ")}")
}
object constrsSyntax  extends SyntaxChecking.SyntaxChecker(constrs){
  override def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (lits.exists(!_.isInstanceOf[Constr]))
      error(s"Constrs elements must be of sort constr, but found ${lits.filter(!_.isInstanceOf[Constr])}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}

case object cons extends Constr(simple(Seq(classOf[Symbol], classOf[Seq[AType]])))

case object infixcon extends Constr(simple(Seq(btype, conop, btype)) orElse simple(Seq(atype, conop, atype)))

case object fieldcon extends Constr(_ => fieldconSyntax){
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

abstract class Deriving extends NodeKind(simple(Seq(classOf[Symbol])) orElse simple(Seq(classOf[Seq[Symbol]])))
object Deriving {
  val deriving = classOf[Deriving]
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

case object fdecl extends NodeKind(simple(Seq(classOf[Symbol], ftype )))

abstract class Ftype(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Ftype {
  val ftype = classOf[Ftype]
}

case object EmptyFtype extends Ftype(simple())

case object fatype extends Ftype(_ => fatypeSyntax)
object fatypeSyntax  extends SyntaxChecking.SyntaxChecker(fatype){
  override def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (lits.drop(1).exists(!_.isInstanceOf[AType]))
      error(s"All Field types of a constructor  must be of sort Atype, but found ${lits.drop(1).filter(!_.isInstanceOf[Symbol])}")
    if ((!lits(0).isInstanceOf[Symbol]))
      error(s"Constructor Type expected of sort Symbol, but found ${lits(0)}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}

case object ftypefun extends Ftype(_ => ftypefunSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.lits(0)} -> ${e.lits(1)}")
}
object ftypefunSyntax  extends SyntaxChecking.SyntaxChecker(ftypefun){
  override def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (!(lits(0) == fatype))
      error(s"All argument types of a constructor  must be of sort FAtype, but found ${lits(0)}")
    if ((!lits(1).isInstanceOf[Ftype]))
      error(s"Return Type expected of sort Ftype, but found ${lits(1)}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}


import incremental.haskell.Pat._
import incremental.haskell.Apat._
import incremental.haskell.Funlhs._
import incremental.haskell.Exp._
import incremental.haskell.OptionKind.option
import incremental.haskell.Funlhs._
import incremental.haskell.RhsA._

abstract class Funlhs(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Funlhs {
  val funlhs = classOf[Funlhs]
}
case object Varfunlhs extends Funlhs(simple(Seq(classOf[Symbol]), apat, apat)) {
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.lits(0)} ${e.kids(0)} { ${e.kids(1)} }")
}
case object opfunlhs extends Funlhs(simple(Seq(varop), pat, pat)) {
  override def toString(e: Node_[_]): Option[String] = Some(s"${e.kids(0)} ${e.lits(0)} ${e.kids(1)}")
}
case object funlhsPat extends Funlhs(simple(funlhs, apat, apat)) {
  override def toString(e: Node_[_]): Option[String] = Some(s"( ${e.kids(0)} ) ${e.kids(1)} { ${e.kids(2)} }")
}

abstract class RhsA(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object RhsA {
  val rhs = classOf[RhsA]
}

import incremental.haskell.DeclsA._
case object Rhs extends RhsA(simple(cExp) orElse (simple(decls, cExp))) {
  override def toString(e: Node_[_]): Option[String] = {
    if (e.lits.size == 0)
      Some(s" = ${e.kids(0)}")
    else
      Some(s" = ${e.kids(0)} where ${e.lits(0)}") //TODO See again if the DECL in where clause is a lit or a kid (since it is a declaration)
  }
}


//=================================================================================================

case object Data extends NodeKind(_ => DataSyntax) {
  override def toString(e: Node_[_]): Option[String] = {
    if (e.lits.size == 1)
      Some(s"data ${e.lits(0)}")
    else if (e.lits.seq.size == 2){
      if (e.lits(0) == context)
        Some(s"data ${e.lits(0)} => ${e.lits(1)}")
      else if (e.lits(1) == constrs)
        Some(s"data ${e.lits(0)} = ${e.lits(1)}")
      else Some(s"data ${e.lits(0)} ${e.lits(1)}")
    }
    else if (e.lits.size == 3) {
      if (e.lits(0) == context && e.lits(2) == constrs)
        Some(s"data ${e.lits(0)} => ${e.lits(1)} = ${e.lits(2)}")
      else if (e.lits(0) == context && e.lits(2).isInstanceOf[Deriving])
        Some(s"data ${e.lits(0)} => ${e.lits(1)} ${e.lits(2)}")
      else Some(s"data ${e.lits(0)} = ${e.lits(1)} ${e.lits(2)}")
    }
    else Some(s"data ${e.lits(0)} => ${e.lits(1)} = ${e.lits(2)} ${e.lits(3)}")
  }
}
object DataSyntax extends SyntaxChecking.SyntaxChecker(Data) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
    if (lits.size == 1 && !(lits(0) == simpletype))
      error(s"Expected simpletype but got ${lits(0)}")
    if (lits.size == 4 && (!(lits(0) == context) || !(lits(0) == simpletype) || !(lits(0) == constrs) || !(lits(0).isInstanceOf[Deriving])))
      error(s"Literals of the data constructor are not of the expected sort!")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}

case object newtype extends NodeKind(_ => newtypeSyntax) {
  override def toString(e: Node_[_]): Option[String] = {
    if (e.lits.size == 2)
      Some(s"newtype ${e.lits(0)} = ${e.lits(1)}")
    else if (e.lits.size == 3){
      if (e.lits(0) == context)
        Some(s"newtype ${e.lits(0)} => ${e.lits(1)} = ${e.lits(2)}")
      else
        Some(s"newtype ${e.lits(0)} = ${e.lits(1)} ${e.lits(2)}")
    }
    else
      Some(s"newtype ${e.lits(0)} => ${e.lits(1)} = ${e.lits(2)} ${e.lits(3)}")
  }
}
object newtypeSyntax extends SyntaxChecking.SyntaxChecker(newtype){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (lits.size == 2 && (lits(0) != simpletype || lits(1) != newconstr))
      error(s"Literals of newtype expected simpletype or newconstr but got ${lits(0)} or ${lits(1)}")
    if (lits.size == 4 && (lits(0) != context || lits(1) != simpletype || lits(1) != newconstr || !(lits(1).isInstanceOf[Deriving])))
      error(s"Literals of newtype are not of the expected sort")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}

import incremental.haskell.ScontextA._
import incremental.haskell.CdeclsA._
case object ClassDecl extends Toplevel(sequence(simple(Seq(classOf[Symbol], classOf[Symbol])), option(simple(cdecls)))
  orElse sequence(simple(Seq(scontext, classOf[Symbol], classOf[Symbol])), option(simple(cdecls)))) {
  override def toString(e: Node_[_]): Option[String] = {
   if (e.lits.size == 2 && e.kids.seq.size == 0)
     Some(s"class ${e.lits(0)} ${e.lits(1)}")
   else if (e.lits.size == 2 && e.kids.seq.size == 1)
       Some(s"class ${e.lits(0)} ${e.lits(1)} where ${e.kids(0)}")
   else if (e.lits.size == 3 && e.kids.seq.size == 0)
       Some(s"class ${e.lits(0)} => ${e.lits(1)} ${e.lits(2)}")
    else
     Some(s"class ${e.lits(0)} => ${e.lits(1)} ${e.lits(2)} where ${e.kids(0)}")
  }
}

import incremental.haskell.Inst._
import incremental.haskell.IdeclsA._
case object InstanceDecl extends Toplevel(sequence(simple(Seq(classOf[Symbol], inst)), option(simple(idecls)))
  orElse sequence(simple(Seq(scontext, classOf[Symbol], inst)), option(simple(idecls)))) {
  override def toString(e: Node_[_]): Option[String] = {
    if (e.lits.size == 2 && e.kids.seq.size == 0)
      Some(s"instance ${e.lits(0)} ${e.lits(1)}")
    else if (e.lits.size == 2 && e.kids.seq.size == 1)
      Some(s"instance ${e.lits(0)} ${e.lits(1)} where ${e.kids(0)}")
    else if (e.lits.size ==3  && e.kids.seq.size == 0)
      Some(s"instance ${e.lits(0)} => ${e.lits(1)} ${e.lits(2)}")
    else
      Some(s"instance ${e.lits(0)} => ${e.lits(1)} ${e.lits(2)} where ${e.kids(0)}")
  }
}

case object default extends Toplevel(_ => DefaultSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"default ( ${e.lits.mkString(", ")} )")
}
object DefaultSyntax extends SyntaxChecking.SyntaxChecker(default) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]){
    if (lits.exists(!_.isInstanceOf[TypeH]))
      error(s"All lits of default must be of sort type haskell, but found ${lits.filter(!_.isInstanceOf[TypeH])}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}


case object ForeignFun extends Toplevel(_ => ForeignFunSyntax) {
  override def toString(e: Node_[_]): Option[String] = Some(s"foreign ${e.lits(0)}")
}
object ForeignFunSyntax extends SyntaxChecking.SyntaxChecker(ForeignFun) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]){
    if (!(lits(0) == fdecl))
      error(s"Foreign function expected of sort fdecl, but got ${lits(0)}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}
 //========================================= DECL ===============================================
import incremental.haskell.Gendecl._

abstract class Declaration(syntaxcheck: SyntaxChecking.SyntaxCheck) extends Toplevel(syntaxcheck)

abstract class DeclsA(syntaxcheck: SyntaxChecking.SyntaxCheck) extends Declaration(syntaxcheck)
object DeclsA {
  val decls = classOf[DeclsA]
}
abstract class Decl(syntaxcheck: SyntaxChecking.SyntaxCheck) extends CdeclsA(syntaxcheck)
object Decl {
  val decl = classOf[Decl]
}

case object Decls extends DeclsA(_ => DeclsSyntax)
object DeclsSyntax extends SyntaxChecking.SyntaxChecker(Decls){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (kids.exists(!_.kind.isInstanceOf[Decl]))
      error(s"All kids must be of sort cdecl, but found ${kids.filter(!_.kind.isInstanceOf[Decl])}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}
case object genDecl extends Decl(simple(gendecl))
case object SomeDecl extends Decl(simple(funlhs, rhs) orElse simple(pat, rhs))

abstract class CdeclsA(syntaxcheck: SyntaxChecking.SyntaxCheck) extends Declaration(syntaxcheck)
object CdeclsA {
  val cdecls = classOf[CdeclsA]
}
abstract class Cdecl(syntaxcheck: SyntaxChecking.SyntaxCheck) extends CdeclsA(syntaxcheck)
object Cdecl {
  val cdecl = classOf[Cdecl]
}

case object Cdecls extends CdeclsA(_ => CdeclsSyntax)
object CdeclsSyntax extends SyntaxChecking.SyntaxChecker(Cdecls){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (kids.exists(!_.kind.isInstanceOf[Cdecl]))
      error(s"All kids must be of sort cdecl, but found ${kids.filter(!_.kind.isInstanceOf[Cdecl])}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}
case object genCdecl extends Cdecl(simple(gendecl))
case object SomeCdecl extends Cdecl(simple(funlhs, rhs) orElse simple(Seq(classOf[Symbol]), rhs))

abstract class Gendecl(syntaxcheck: SyntaxChecking.SyntaxCheck) extends Declaration(syntaxcheck)
object Gendecl {
  val gendecl = classOf[Gendecl]
}
case object TypeSig extends Gendecl(_ => TypeSigSyntax){
  override def toString(e: Node_[_]): Option[String] = {
    if (e.lits.size == 2)
      Some(s"${e.lits(0)} :: ${e.lits(1)}")
    else
      Some(s"${e.lits(0)} :: ${e.lits(1)} => ${e.lits(2)}")
  }
}
object TypeSigSyntax extends SyntaxChecking.SyntaxChecker(TypeSig){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (lits.size == 2 && (!(lits(0).isInstanceOf[Symbol]) || !(lits(1).isInstanceOf[TypeH])))
      error(s"Type signature expects a variable and a type but got ${lits(0)} or ${lits(1)}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}
case object FixityDecl extends Gendecl(simple()) //TODO See this again I do not know how to express Fixity

case object EmptyCdecl extends Gendecl(simple())


abstract class IdeclsA(syntaxcheck: SyntaxChecking.SyntaxCheck) extends Declaration(syntaxcheck)
object IdeclsA {
  val idecls = classOf[IdeclsA]
}
abstract class Idecl(syntaxcheck: SyntaxChecking.SyntaxCheck) extends IdeclsA(syntaxcheck)
object Idecl {
  val idecl = classOf[Idecl]
}

case object Idecls extends IdeclsA(_ => IdeclsSyntax)
object IdeclsSyntax extends SyntaxChecking.SyntaxChecker(Idecls){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (kids.exists(!_.kind.isInstanceOf[Idecl]))
      error(s"All kids must be of sort cdecl, but found ${kids.filter(!_.kind.isInstanceOf[Idecl])}")
  }
  def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]) {}
}
case object SomeIdecl extends Idecl(simple(funlhs, rhs) orElse simple(Seq(classOf[Symbol]), rhs))
case object EmptyIdecl extends Idecl(simple())
