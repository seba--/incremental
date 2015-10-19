package incremental.java.syntax

import incremental.Node._
import incremental.java.JavaCheck._
import incremental.java.syntax.JavaSyntaxChecker._
import incremental.{NodeKind, SyntaxChecking}

/**
 * Created by qwert on 27.05.15.
 */

// AnnotationTypes
trait NT_AnnoDec extends NT_AnnoElemDec with NT_InterfaceDec
trait NT_AnnoDecHead
case object AnnoDec extends NodeKind_TMP[Result](_ => AnnoDecSyntax) with NT_AnnoDec
case object AnnoDecHead extends NodeKind_TMP[Result](litsFollowedBy(classOf[InterfaceMod], classOf[String]) andAlso unsafeAllKids(classOf[NT_Anno])) with NT_AnnoDecHead

trait NT_AnnoElemDec
trait NT_DefaultVal
case object AnnoMethodDec extends NodeKind_TMP[Result](_ => AnnoMethodDecSyntax) with NT_AnnoElemDec
case object DefaultVal extends NodeKind_TMP[Result](noLits andAlso unsafeKids(Seq(classOf[NT_ElemVal]))) with NT_DefaultVal

// AbstractMethodDeclarations
trait NT_AbstractMethodDec extends NT_InterfaceMemberDec
case object AbstractMethodDec extends NodeKind_TMP[Result](_ => AbstractMethodDecSyntax) with NT_AbstractMethodDec
case object DeprAbstractMethodDec extends NodeKind_TMP[Result](_ => DeprAbstractMethodDecSyntax) with NT_AbstractMethodDec

// ConstantDeclarations
trait NT_ConstantDec extends NT_InterfaceMemberDec with NT_AnnoElemDec
case object ConstantDec extends NodeKind_TMP[Result](litsFollowedBy(classOf[ConstantMod], classOf[Type]) andAlso nonEmptyKids andAlso unsafeAllKids(classOf[NT_Anno], VarDec.getClass)) with NT_ConstantDec // _ => ConstantDecSyntax

// InterfaceDeclarations
trait NT_InterfaceDec extends NT_AnnoElemDec with NT_InterfaceMemberDec with NT_ClassMemberDec with NT_TypeDec
trait NT_InterfaceDecHead
trait NT_ExtendsInterfaces
trait NT_InterfaceMemberDec

case object InterfaceDec extends NodeKind_TMP[Result](uFollowedByKids(InterfaceDecHead.getClass, classOf[NT_InterfaceMemberDec])) with NT_InterfaceDec
case object InterfaceDecHead extends NodeKind_TMP[Result](/* kids */(unsafeAllKids(classOf[NT_Anno]) orElse uKidsFollowedBy(classOf[NT_Anno], ExtendsInterfaces.getClass)) andAlso
                                              /* lits */(litsFollowedBy(classOf[InterfaceMod], classOf[String]) orElse
                                                         litsFollowedBy(classOf[InterfaceMod], Seq(classOf[String], classOf[TypeParams])))) with NT_InterfaceDecHead
case object ExtendsInterfaces extends NodeKind_TMP[Result](allLits(classOf[InterfaceType]) andAlso nonEmptyLits) with NT_ExtendsInterfaces

case object Semicolon extends NodeKind_TMP[Result](simple()) with NT_InterfaceMemberDec with NT_ClassMemberDec with NT_TypeDec with NT_AnnoElemDec