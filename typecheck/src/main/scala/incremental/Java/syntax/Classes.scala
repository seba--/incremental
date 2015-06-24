package incremental.Java.syntax

import incremental.Node._
import incremental.{NodeKind, SyntaxChecking}
import incremental.Java.syntax.JavaSyntaxChecker._

/**
 * Created by qwert on 06.05.15.
 */

import Stm._
import Expr._
import ArrayInit._

// FieldDeclarations
trait NT_FieldDec
trait NT_VarDec
trait NT_VarDecId
trait NT_VarInit
trait NT_Dim

case object FieldDec extends NodeKind(_ => FieldDecSyntax) with NT_FieldDec

case object VarDec extends NodeKind((noKids andAlso lits(Seq(classOf[NT_VarDecId]))) orElse
                                    (lits(Seq(classOf[NT_VarDecId])) andAlso unsafeKids(Seq(classOf[NT_VarInit])))) with NT_VarDec

case class ArrayVarDecId(id: String, dims: Seq[NT_Dim]) extends NT_VarDecId

case class DimV() extends NT_Dim

// FormalParam
trait NT_FormalParam
abstract class FormalParam(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object Param extends FormalParam(k => new FormalParamSyntax(k)) with NT_FormalParam
case object VarArityParam extends FormalParam(k => new FormalParamSyntax(k)) with NT_FormalParam

trait Throws
trait ExceptionType
case class ThrowsDec(ex: Seq[ExceptionType]) extends Throws

// EnumDeclarations
trait NT_EnumDec
trait NT_EnumDecHead
trait NT_EnumBody
trait NT_EnumConst
trait NT_EnumConstArgs
trait NT_EnumBodyDecs

case object EnumDec extends NodeKind(noLits andAlso unsafeKids(Seq(classOf[NT_EnumDecHead], classOf[NT_EnumBody]))) with NT_EnumDec
case object EnumDecHead extends NodeKind(litsFollowedBy(classOf[ClassMod], classOf[String]) andAlso unsafeAllKids(classOf[NT_Anno])) with NT_EnumDecHead // TODO: lits: orElse Interfaces
case object EnumBody extends NodeKind((noLits andAlso unsafeAllKids(classOf[NT_EnumConst])) orElse
                                      (noLits andAlso uKidsFollowedBy(classOf[NT_EnumConst], classOf[NT_EnumBodyDecs]))) with NT_EnumBody
case object EnumConst extends NodeKind(simple(Seq(classOf[String])) orElse
                                       (lits(Seq(classOf[String])) andAlso allKids(cExpr))) with NT_EnumConst
// TODO: (lits(Seq(classOf[String])) andAlso unsafeKids(Seq(classOf[NT_ClassBody])))
// TODO: (lits(Seq(classOf[String])) andAlso uKidsFollowedBy(cExpr, classOf[NT_ClassBody]))
case object EnumBodyDecs extends NodeKind(simple()) with NT_EnumBodyDecs // TODO: ";" ClassBodyDec* -> EnumBodyDecs

// ConstructorDeclarations
trait NT_ConstrDec
trait NT_ConstrBody
trait NT_ConstrHead
trait NT_ConstrInv

case object ConstrDec extends NodeKind(noLits andAlso unsafeKids(Seq(classOf[NT_ConstrHead], classOf[NT_ConstrBody]))) with NT_ConstrDec
case object ConstrDecHead extends NodeKind((litsFollowedBy(classOf[ConstrMod], classOf[String]) orElse
                                           litsFollowedBy(classOf[ConstrMod], Seq(classOf[TypeParams], classOf[String])) orElse
                                           litsFollowedBy(classOf[ConstrMod], Seq(classOf[String], classOf[Throws])) orElse
                                           litsFollowedBy(classOf[ConstrMod], Seq(classOf[TypeParams], classOf[String], classOf[Throws])))
                                          andAlso
                                          unsafeAllKids(classOf[NT_Anno], classOf[NT_FormalParam])) with NT_ConstrHead
case object ConstrBody extends NodeKind(uFollowedByKids(classOf[NT_ConstrInv], classOf[NT_BlockStm])) with NT_ConstrBody
case object AltConstrInv extends NodeKind((noLits orElse lits(Seq(classOf[TypeArgs]))) andAlso allKids(cExpr)) with NT_ConstrInv
case object SuperConstrInv extends NodeKind((noLits orElse lits(Seq(classOf[TypeArgs]))) andAlso allKids(cExpr)) with NT_ConstrInv
case object QSuperConstrInv extends NodeKind((noLits orElse lits(Seq(classOf[TypeArgs]))) andAlso allKids(cExpr) andAlso nonEmptyKids) with NT_ConstrInv

// StaticInitializers
trait NT_StaticInit
case object StaticInit extends NodeKind(simple(Block.getClass)) with NT_StaticInit

// InstanceInitializers
trait NT_InstanceInit
case object InstanceInit extends NodeKind(simple(Block.getClass)) with NT_InstanceInit

///////////////////////////

// Method Dec
abstract class MethodDec(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object MethodDeclaration extends MethodDec(simple(Seq(classOf[MethodDecHead], classOf[MethodBody])))

abstract class MethodDecHead(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object MethodDeclarationHead extends MethodDecHead(_ => MethodDecHeadSyntax)
case object DeprMethodDeclarationHead extends MethodDecHead(_ => DeprMethodDecHeadSyntax)

abstract class MethodBody(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object NoMethodBody extends MethodBody(simple())
case object MethodBody extends MethodBody(simple(Block.getClass))