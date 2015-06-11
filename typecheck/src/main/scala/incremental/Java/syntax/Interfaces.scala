package incremental.Java.syntax

import incremental.Node._
import incremental.Java.syntax.JavaSyntaxChecker._
import incremental.{NodeKind, SyntaxChecking}

/**
 * Created by qwert on 27.05.15.
 */

// AnnotationTypes
trait NT_AnnoDec
trait NT_AnnoDecHead
case object AnnoDec extends NodeKind(_ => AnnoDecSyntax) with NT_AnnoDec
case object AnnoDecHead extends NodeKind(ignore) with NT_AnnoDecHead

trait NT_AnnoElemDec
trait NT_DefaultVal
case object AnnoMethodDec extends NodeKind(_ => AnnoMethodDecSyntax) with NT_AnnoElemDec
case object DefaultVal extends NodeKind(noLits andAlso unsafeKids(Seq(classOf[NT_ElemVal]))) with NT_DefaultVal

/////////////

// AbstractMethodDeclarations
abstract class AbstractMethodDec(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object AbstractMethodDec extends AbstractMethodDec(_ => AbstractMethodDecSyntax) // TODO: name conflict?
case object DeprAbstractMethodDec extends AbstractMethodDec(_ => DeprAbstractMethodDecSyntax)

// ConstantDeclarations
case object ConstantDec extends NodeKind(_ => ConstantDecSyntax)

// InterfaceDeclarations
abstract class InterfaceDec(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
//case object InterfaceDec extends InterfaceDec(followedByKids(InterfaceDecHead.getClass, classOf[InterfaceMemberDec]))

case object InterfaceDecHead extends NodeKind(_ => InterfaceDecHeadSyntax)
case object ExtendsInterfaces extends NodeKind(allLits(classOf[InterfaceType]) andAlso nonEmptyLits)

abstract class InterfaceMemberDec(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)