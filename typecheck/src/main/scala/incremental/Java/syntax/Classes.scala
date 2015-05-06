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

abstract class VarDec(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object VarDec{
  val cVarDec = classOf[VarDec]
}

// TODO: nodekind for vardecid

case object VariableDec extends VarDec(simple(Seq(classOf[String])) orElse (_ => ArrayVarDecSyntax))
case object VariableDecInit extends VarDec(simple(Seq(classOf[String], cExpr)) orElse simple(Seq(classOf[String], cArrayInit)) orElse (_ => ArrayVarDecInitSyntax))

// Method Dec
abstract class MethodDec(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object MethodDeclaration extends MethodDec(simple(Seq(classOf[MethodDecHead], classOf[MethodBody])))

abstract class MethodDecHead(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object MethodDeclarationHead extends MethodDecHead(simple()) // TODO
case object DeprMethodDeclarationHead extends MethodDecHead(simple())

trait ResultType
case class ResType(t: Type) extends ResultType
case class Void() extends ResultType

// FormalParam

abstract class MethodBody(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)