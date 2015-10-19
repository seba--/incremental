package incremental.java.syntax

import constraints.javacons.Constraint
import incremental.Node._
import incremental.java.JavaCheck._
import incremental.java.syntax.expr.Expr
import incremental.{NodeKind, SyntaxChecking}
import incremental.java.syntax.JavaSyntaxChecker._

/**
 * Created by qwert on 06.05.15.
 */

import Stm._
import Expr._
import ArrayInit._

// FieldDeclarations
trait NT_FieldDec extends NT_ClassMemberDec
trait NT_VarDec
trait NT_VarDecId
trait NT_VarInit
trait NT_Dim

case object FieldDec extends NodeKind_TMP[Constraint, Result](_ => FieldDecSyntax) with NT_FieldDec

case object VarDec extends NodeKind_TMP[Constraint, Result]((noKids andAlso lits(Seq(classOf[NT_VarDecId]))) orElse
                                    (lits(Seq(classOf[NT_VarDecId])) andAlso unsafeKids(Seq(classOf[NT_VarInit])))) with NT_VarDec

case class ArrayVarDecId(id: String, dims: Seq[NT_Dim]) extends NT_VarDecId

case class DimV() extends NT_Dim

// FormalParam
trait NT_FormalParam
abstract class FormalParam(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind_TMP[Constraint, Result](syntaxcheck)
case object Param extends FormalParam(k => new FormalParamSyntax(k)) with NT_FormalParam
case object VarArityParam extends FormalParam(k => new FormalParamSyntax(k)) with NT_FormalParam

trait Throws
trait ExceptionType
case class ThrowsDec(ex: Seq[ExceptionType]) extends Throws

// EnumDeclarations
trait NT_EnumDec extends NT_ClassDec with NT_AnnoElemDec
trait NT_EnumDecHead
trait NT_EnumBody
trait NT_EnumConst
trait NT_EnumConstArgs
trait NT_EnumBodyDecs

case object EnumDec extends NodeKind_TMP[Constraint, Result](noLits andAlso unsafeKids(Seq(classOf[NT_EnumDecHead], classOf[NT_EnumBody]))) with NT_EnumDec

case object EnumDecHead extends NodeKind_TMP[Constraint, Result]((litsFollowedBy(classOf[ClassMod], classOf[String]) orElse litsFollowedBy(classOf[ClassMod], Seq(classOf[String], classOf[Interfaces])))
                                         andAlso unsafeAllKids(classOf[NT_Anno])) with NT_EnumDecHead

case object EnumBody extends NodeKind_TMP[Constraint, Result]((noLits andAlso unsafeAllKids(classOf[NT_EnumConst])) orElse
                                      (noLits andAlso uKidsFollowedBy(classOf[NT_EnumConst], classOf[NT_EnumBodyDecs]))) with NT_EnumBody
case object EnumConst extends NodeKind_TMP[Constraint, Result](simple(Seq(classOf[String])) orElse
                                       (lits(Seq(classOf[String])) andAlso allKids(cExpr)) orElse
                                       (lits(Seq(classOf[String])) andAlso unsafeKids(Seq(classOf[NT_ClassBody]))) orElse
                                       (lits(Seq(classOf[String])) andAlso uKidsFollowedBy(cExpr, classOf[NT_ClassBody]))) with NT_EnumConst
case object EnumBodyDecs extends NodeKind_TMP[Constraint, Result](unsafeAllKids(classOf[NT_ClassBodyDec])) with NT_EnumBodyDecs

// ConstructorDeclarations
trait NT_ConstrDec extends NT_ClassBodyDec
trait NT_ConstrBody
trait NT_ConstrHead
trait NT_ConstrInv

case object ConstrDec extends NodeKind_TMP[Constraint, Result](noLits andAlso unsafeKids(Seq(classOf[NT_ConstrHead], classOf[NT_ConstrBody]))) with NT_ConstrDec
case object ConstrDecHead extends NodeKind_TMP[Constraint, Result]((litsFollowedBy(classOf[ConstrMod], classOf[String]) orElse
                                           litsFollowedBy(classOf[ConstrMod], Seq(classOf[TypeParams], classOf[String])) orElse
                                           litsFollowedBy(classOf[ConstrMod], Seq(classOf[String], classOf[Throws])) orElse
                                           litsFollowedBy(classOf[ConstrMod], Seq(classOf[TypeParams], classOf[String], classOf[Throws])))
                                          andAlso
                                          unsafeAllKids(classOf[NT_Anno], classOf[NT_FormalParam])) with NT_ConstrHead
case object ConstrBody extends NodeKind_TMP[Constraint, Result](uFollowedByKids(classOf[NT_ConstrInv], classOf[NT_BlockStm])) with NT_ConstrBody
case object AltConstrInv extends NodeKind_TMP[Constraint, Result]((noLits orElse lits(Seq(classOf[TypeArgs]))) andAlso allKids(cExpr)) with NT_ConstrInv
case object SuperConstrInv extends NodeKind_TMP[Constraint, Result]((noLits orElse lits(Seq(classOf[TypeArgs]))) andAlso allKids(cExpr)) with NT_ConstrInv
case object QSuperConstrInv extends NodeKind_TMP[Constraint, Result]((noLits orElse lits(Seq(classOf[TypeArgs]))) andAlso allKids(cExpr) andAlso nonEmptyKids) with NT_ConstrInv

// StaticInitializers
trait NT_StaticInit extends NT_ClassBodyDec
case object StaticInit extends NodeKind_TMP[Constraint, Result](simple(Block.getClass)) with NT_StaticInit

// InstanceInitializers
trait NT_InstanceInit extends NT_ClassBodyDec
case object InstanceInit extends NodeKind_TMP[Constraint, Result](simple(Block.getClass)) with NT_InstanceInit

// MethodDeclarations
trait NT_MethodDec extends NT_ClassMemberDec
trait NT_MethodDecHead

case object MethodDec extends NodeKind_TMP[Constraint, Result](noLits andAlso unsafeKids(Seq(classOf[NT_MethodDecHead], classOf[NT_MethodBody]))) with NT_MethodDec
case object MethodDecHead extends NodeKind_TMP[Constraint, Result]((litsFollowedBy(classOf[MethodMod], Seq(classOf[ResultType], classOf[String])) orElse
                                            litsFollowedBy(classOf[MethodMod], Seq(classOf[TypeParams], classOf[ResultType], classOf[String])) orElse
                                            litsFollowedBy(classOf[MethodMod], Seq(classOf[ResultType], classOf[String], classOf[Throws])) orElse
                                            litsFollowedBy(classOf[MethodMod], Seq(classOf[TypeParams], classOf[ResultType], classOf[String], classOf[Throws])))
                                           andAlso unsafeAllKids(classOf[NT_Anno], classOf[NT_FormalParam])) with NT_MethodDecHead
case object DeprMethodDecHead extends NodeKind_TMP[Constraint, Result](simple()) with NT_MethodDecHead // TODO: ( Anno | MethodMod )* TypeParams? ResultType Id "(" {FormalParam ","}* ")" Dim+ Throws?

trait NT_MethodBody
case object NoMethodBody extends NodeKind_TMP[Constraint, Result](simple()) with NT_MethodBody

// ClassDeclarations
trait NT_ClassDec extends NT_ClassMemberDec with NT_TypeDec with NT_InterfaceMemberDec with NT_AnnoElemDec
trait NT_ClassBody
trait NT_ClassDecHead

case object ClassDec extends NodeKind_TMP[Constraint, Result](noLits andAlso unsafeKids(Seq(classOf[NT_ClassDecHead], classOf[NT_ClassBody]))) with NT_ClassDec
case object ClassBody extends NodeKind_TMP[Constraint, Result](noLits andAlso unsafeAllKids(classOf[NT_ClassBodyDec])) with NT_ClassBody
case object ClassDecHead extends NodeKind_TMP[Constraint, Result](unsafeAllKids(classOf[NT_Anno]) andAlso
                                          (litsFollowedBy(classOf[ClassMod], classOf[String]) orElse
                                           litsFollowedBy(classOf[ClassMod], Seq(classOf[String], classOf[TypeParams])) orElse
                                           litsFollowedBy(classOf[ClassMod], Seq(classOf[String], classOf[Super])) orElse
                                           litsFollowedBy(classOf[ClassMod], Seq(classOf[String], classOf[Interfaces])) orElse
                                           litsFollowedBy(classOf[ClassMod], Seq(classOf[String], classOf[TypeParams], classOf[Super])) orElse
                                           litsFollowedBy(classOf[ClassMod], Seq(classOf[String], classOf[TypeParams], classOf[Interfaces])) orElse
                                           litsFollowedBy(classOf[ClassMod], Seq(classOf[String], classOf[Super], classOf[Interfaces])) orElse
                                           litsFollowedBy(classOf[ClassMod], Seq(classOf[String], classOf[TypeParams], classOf[Super], classOf[Interfaces])))) with NT_ClassDecHead

trait Super
case class SuperDec(t: ClassType) extends Super

trait Interfaces
case class ImplementsDec(ifaces: Seq[InterfaceType]) extends Interfaces

trait NT_ClassBodyDec
trait NT_ClassMemberDec extends NT_ClassBodyDec

trait NT_TypeDec