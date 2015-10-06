package incremental.java.syntax

import incremental.{Node_, SyntaxChecking, NodeKind}
import incremental.Node._
import incremental.java.syntax.JavaSyntaxChecker._
import incremental.java.JavaCheck._

/**
 * Created by qwert on 09.06.15.
 */

trait NT_Anno extends NT_ElemVal
trait NT_ElemVal
trait NT_ElemValPair

// TODO: remove this, just for intermediate purpose (compilation)
abstract class NodeKind_TMP[T](syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind[T](syntaxcheck){
  def check(lits: Seq[Any], kids: Seq[Node_[T]]): T = ???
}

case object Anno extends NodeKind_TMP[StepResult](lits(Seq(classOf[TypeName])) andAlso unsafeAllKids(classOf[NT_ElemValPair])) with NT_Anno
case object SingleElemAnno extends NodeKind_TMP[StepResult](lits(Seq(classOf[TypeName])) andAlso unsafeKids(Seq(classOf[NT_ElemVal]))) with NT_Anno
case object MarkerAnno extends NodeKind_TMP[StepResult](simple(Seq(classOf[TypeName]))) with NT_Anno

case object ElemValPair extends NodeKind_TMP[StepResult](lits(Seq(classOf[String])) andAlso unsafeKids(Seq(classOf[NT_ElemVal]))) with NT_ElemValPair

case object ElemValArrayInit extends NodeKind_TMP[StepResult](unsafeAllKids(classOf[NT_ElemVal])) with NT_ElemVal