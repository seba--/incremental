package incremental.Java.syntax

import incremental.Node._
import incremental.Java.syntax.JavaSyntaxChecker._
import incremental.{NodeKind, SyntaxChecking}

/**
 * Created by qwert on 27.05.15.
 */

// AnnotationTypes
trait NT_AnnoDec extends NT_AnnoElemDec with NT_InterfaceDec
trait NT_AnnoDecHead
case object AnnoDec extends NodeKind(_ => AnnoDecSyntax) with NT_AnnoDec
case object AnnoDecHead extends NodeKind(litsFollowedBy(classOf[InterfaceMod], classOf[String]) andAlso unsafeAllKids(classOf[NT_Anno])) with NT_AnnoDecHead

trait NT_AnnoElemDec
trait NT_DefaultVal
case object AnnoMethodDec extends NodeKind(_ => AnnoMethodDecSyntax) with NT_AnnoElemDec
case object DefaultVal extends NodeKind(noLits andAlso unsafeKids(Seq(classOf[NT_ElemVal]))) with NT_DefaultVal

// AbstractMethodDeclarations
trait NT_AbstractMethodDec extends NT_InterfaceMemberDec
case object AbstractMethodDec extends NodeKind(_ => AbstractMethodDecSyntax) with NT_AbstractMethodDec
case object DeprAbstractMethodDec extends NodeKind(_ => DeprAbstractMethodDecSyntax) with NT_AbstractMethodDec

// ConstantDeclarations
trait NT_ConstantDec extends NT_InterfaceMemberDec
case object ConstantDec extends NodeKind(litsFollowedBy(classOf[ConstantMod], classOf[Type]) andAlso nonEmptyKids andAlso unsafeAllKids(classOf[NT_Anno], VarDec.getClass)) with NT_ConstantDec // _ => ConstantDecSyntax

// InterfaceDeclarations
trait NT_InterfaceDec extends NT_AnnoElemDec with NT_InterfaceMemberDec
trait NT_InterfaceDecHead
trait NT_ExtendsInterfaces
trait NT_InterfaceMemberDec

case object InterfaceDec extends NodeKind(uFollowedByKids(InterfaceDecHead.getClass, classOf[NT_InterfaceMemberDec])) with NT_InterfaceDec
case object InterfaceDecHead extends NodeKind(/* kids */(unsafeAllKids(classOf[NT_Anno]) orElse uKidsFollowedBy(classOf[NT_Anno], ExtendsInterfaces.getClass)) andAlso
                                              /* lits */(litsFollowedBy(classOf[InterfaceMod], classOf[String]) orElse
                                                         litsFollowedBy(classOf[InterfaceMod], Seq(classOf[String], classOf[TypeParams])))) with NT_InterfaceDecHead
case object ExtendsInterfaces extends NodeKind(allLits(classOf[InterfaceType]) andAlso nonEmptyLits) with NT_ExtendsInterfaces

case object Semicolon extends NodeKind(simple()) with NT_InterfaceMemberDec