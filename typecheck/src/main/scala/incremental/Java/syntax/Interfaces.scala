package incremental.Java.syntax

import incremental.Node._
import incremental.Java.syntax.JavaSyntaxChecker._
import incremental.{NodeKind, SyntaxChecking}

/**
 * Created by qwert on 27.05.15.
 */

// AnnotationTypes
trait NT_AnnoDec extends NT_AnnoElemDec
trait NT_AnnoDecHead
case object AnnoDec extends NodeKind(_ => AnnoDecSyntax) with NT_AnnoDec
case object AnnoDecHead extends NodeKind(litsFollowedBy(classOf[InterfaceMod], classOf[String]) andAlso unsafeAllKids(classOf[NT_Anno])) with NT_AnnoDecHead

trait NT_AnnoElemDec
trait NT_DefaultVal
case object AnnoMethodDec extends NodeKind(_ => AnnoMethodDecSyntax) with NT_AnnoElemDec
case object DefaultVal extends NodeKind(noLits andAlso unsafeKids(Seq(classOf[NT_ElemVal]))) with NT_DefaultVal

/////////////

// AbstractMethodDeclarations
trait NT_AbstractMethodDec
case object AbstractMethodDec extends NodeKind(_ => AbstractMethodDecSyntax) with NT_AbstractMethodDec
case object DeprAbstractMethodDec extends NodeKind(_ => DeprAbstractMethodDecSyntax) with NT_AbstractMethodDec

// ConstantDeclarations
case object ConstantDec extends NodeKind(_ => ConstantDecSyntax)

// InterfaceDeclarations
abstract class InterfaceDec(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
//case object InterfaceDec extends InterfaceDec(followedByKids(InterfaceDecHead.getClass, classOf[InterfaceMemberDec]))

case object InterfaceDecHead extends NodeKind(_ => InterfaceDecHeadSyntax)
case object ExtendsInterfaces extends NodeKind(allLits(classOf[InterfaceType]) andAlso nonEmptyLits)

abstract class InterfaceMemberDec(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)