package incremental.java.syntax

import incremental.NodeKind
import incremental.Node._
import incremental.java.syntax.JavaSyntaxChecker._

/**
 * Created by qwert on 09.06.15.
 */

trait NT_Anno extends NT_ElemVal
trait NT_ElemVal
trait NT_ElemValPair

case object Anno extends NodeKind(lits(Seq(classOf[TypeName])) andAlso unsafeAllKids(classOf[NT_ElemValPair])) with NT_Anno
case object SingleElemAnno extends NodeKind(lits(Seq(classOf[TypeName])) andAlso unsafeKids(Seq(classOf[NT_ElemVal]))) with NT_Anno
case object MarkerAnno extends NodeKind(simple(Seq(classOf[TypeName]))) with NT_Anno

case object ElemValPair extends NodeKind(lits(Seq(classOf[String])) andAlso unsafeKids(Seq(classOf[NT_ElemVal]))) with NT_ElemValPair

case object ElemValArrayInit extends NodeKind(unsafeAllKids(classOf[NT_ElemVal])) with NT_ElemVal