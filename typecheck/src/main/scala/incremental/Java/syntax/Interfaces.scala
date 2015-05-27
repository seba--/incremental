package incremental.Java.syntax

import incremental.Node._
import incremental.Java.syntax.JavaSyntaxChecker._
import incremental.{NodeKind, SyntaxChecking}

/**
 * Created by qwert on 27.05.15.
 */

// AnnotationTypes
case class AnnoDec(head: AnnoDecHead, elems: Seq[AnnoElemDec])
case class AnnoDecHead(annos: Seq[Anno], ifacemods: Seq[InterfaceMod], id: String)

trait AnnoElemDec
case class AnnoMethodDec(methodMods: Seq[AbstractMethodMod], t: Type, id: String, defaultVal: Option[DefaultVal]) extends AnnoElemDec
case class Semicolon() extends AnnoElemDec
case class DefaultVal(elemVal: ElemVal)

// AbstractMethodDeclarations
abstract class AbstractMethodDec(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object AbstractMethodDec extends AbstractMethodDec(_ => AbstractMethodDecSyntax) // TODO: name conflict?
case object DeprAbstractMethodDec extends AbstractMethodDec(_ => DeprAbstractMethodDecSyntax)

// ConstantDeclarations
case object ConstantDec extends NodeKind(_ => ConstantDecSyntax)

// InterfaceDeclarations
abstract class InterfaceDec(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object InterfaceDec extends InterfaceDec(followedByKids(classOf[InterfaceDecHead.type], classOf[InterfaceMemberDec]))

case object InterfaceDecHead extends NodeKind(_ => InterfaceDecHeadSyntax)
case object ExtendsInterfaces extends NodeKind(allLits(classOf[InterfaceType]) andAlso nonEmptyLits)

abstract class InterfaceMemberDec(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)