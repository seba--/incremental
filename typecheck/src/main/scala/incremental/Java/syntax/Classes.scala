package incremental.Java.syntax

import incremental.Node._
import incremental.{NodeKind, SyntaxChecking}

/**
 * Created by qwert on 06.05.15.
 */


import Stm._
import Expr._
import ArrayInit._

// Field Dec
abstract class FieldDec(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object FieldDec extends FieldDec(_ => FieldDecSyntax) // TODO: name conflict?

trait FieldMod // Public, Protected, Private, Static, Final, Transient, Volatile

abstract class VarDec(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object VarDec{
  val cVarDec = classOf[VarDec]
}
abstract class VarDecId(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
abstract class VarInit(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)

case object VarDecl extends VarDec(simple(classOf[VarDecId]) orElse simple(Seq(classOf[VarDecId], classOf[VarInit])))
//case object VariableDec extends VarDec(simple(Seq(classOf[String])) orElse (_ => ArrayVarDecSyntax))
//case object VariableDecInit extends VarDec(simple(Seq(classOf[String], cExpr)) orElse simple(Seq(classOf[String], cArrayInit)) orElse (_ => ArrayVarDecInitSyntax))

case object VarDecId extends VarDecId(simple(Seq(classOf[String])))
case object ArrayVarDecId extends VarDecId(_ => ArrayVarDecIdSyntax)

case object VarInit extends VarInit(simple(cExpr) orElse simple(cArrayInit))

// Method Dec
abstract class MethodDec(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object MethodDeclaration extends MethodDec(simple(Seq(classOf[MethodDecHead], classOf[MethodBody])))

abstract class MethodDecHead(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object MethodDeclarationHead extends MethodDecHead(_ => MethodDecHeadSyntax)
case object DeprMethodDeclarationHead extends MethodDecHead(_ => DeprMethodDecHeadSyntax)

abstract class MethodBody(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object NoMethodBody extends MethodBody(simple())
case object MethodBody extends MethodBody(simple(classOf[Block]))

trait ResultType
case class ResType(t: Type) extends ResultType
case class Void() extends ResultType

// FormalParam
abstract class FormalParam(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object Param extends FormalParam(simple(Seq(classOf[Type], classOf[VarDecId]))) // TODO: (Anno | VarMod)*
case object VarArityParam extends FormalParam(simple(Seq(classOf[Type], classOf[VarDecId])))

trait VarMod // Final
trait MethodMod // Public, Protected, Private, Abstract, Static, Final, Synchronized, Native, StrictFP

trait Throws
trait ExceptionType extends ClassType
case class ThrowsDec(ex: Seq[ExceptionType]) extends Throws

// Instance Initializers
abstract class InstanceInit(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object InstanceInit extends InstanceInit(simple(classOf[Block]))

// Static Initializers
abstract class StaticInit(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object StaticInit extends StaticInit(simple(classOf[Block]))

// Constructor Declarations
abstract class ConstrDec(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
abstract class ConstrBody(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
abstract class ConstrHead(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
abstract class ConstrInv(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)

case object ConstrDec extends ConstrDec(simple(Seq(classOf[ConstrHead], classOf[ConstrBody])))
case object ConstrDecHead extends ConstrHead(_ => ConstrDecHeadSyntax)
case object ConstrBody extends ConstrBody(_ => ConstrBodySyntax)
case object AltConstrInv extends ConstrInv(k => ConstrInvSyntax(k))
case object SuperConstrInv extends ConstrInv(k => ConstrInvSyntax(k))
case object QSuperConstrInv extends ConstrInv(_ => QConstrInvSyntax)

trait ConstrMod // Public, Protected, Private