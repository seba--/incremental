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

// Method Dec
abstract class MethodDec(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object MethodDeclaration extends MethodDec(simple(Seq(classOf[MethodDecHead], classOf[MethodBody])))

abstract class MethodDecHead(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object MethodDeclarationHead extends MethodDecHead(_ => MethodDecHeadSyntax)
case object DeprMethodDeclarationHead extends MethodDecHead(_ => DeprMethodDecHeadSyntax)

abstract class MethodBody(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object NoMethodBody extends MethodBody(simple())
case object MethodBody extends MethodBody(simple(classOf[Block]))

// FormalParam
trait NT_FormalParam
abstract class FormalParam(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object Param extends FormalParam(_ => new FormalParamSyntax(Param)) with NT_FormalParam
case object VarArityParam extends FormalParam(_ => new FormalParamSyntax(VarArityParam)) with NT_FormalParam

trait Throws
trait ExceptionType
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

case object ConstrDec extends ConstrDec(simple(classOf[ConstrHead], classOf[ConstrBody]))
case object ConstrDecHead extends ConstrHead(_ => ConstrDecHeadSyntax)
case object ConstrBody extends ConstrBody(_ => ConstrBodySyntax)
case object AltConstrInv extends ConstrInv(k => ConstrInvSyntax(k))
case object SuperConstrInv extends ConstrInv(k => ConstrInvSyntax(k))
case object QSuperConstrInv extends ConstrInv(_ => QConstrInvSyntax)

trait ConstrMod // Public, Protected, Private