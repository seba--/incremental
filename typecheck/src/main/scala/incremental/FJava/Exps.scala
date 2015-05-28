package incremental.FJava

import constraints.equality.Type
import incremental.{NodeKind, SyntaxChecking}
import incremental.Node._
import incremental.{Node_, SyntaxChecking}

/**
 * Created by lirakuci on 3/10/15.
 */

case object ClassDec extends NodeKind(_ => ClassSyntax)
case object FieldDec extends NodeKind(_ => FieldSyntax)
case object MethodDec extends NodeKind(_ => MethodSyntax)

abstract class Exp(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Exp {
  val cExp = classOf[Exp]
}
import Exp._

case object Num extends Exp(simple(Seq(classOf[Integer])))
case object Add extends Exp(simple(cExp, cExp))
case object Mul extends Exp(simple(cExp, cExp))

case object Var  extends Exp(simple(Seq(classOf[Symbol])))
case object Fields extends Exp(simple(Seq(classOf[Symbol]), cExp))
case object New extends Exp(_ => NewSyntax)
case object UCast extends Exp(simple(Seq(classOf[CName]),cExp))
case object Invk extends Exp(_ =>InvkSyntax)
case object TClass extends Exp(simple(cExp))
case object DCast extends Exp(simple(cExp))
case object SCast extends Exp(simple(cExp))

object InvkSyntax extends SyntaxChecking.SyntaxChecker(Invk) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
  }
}

object NewSyntax extends SyntaxChecking.SyntaxChecker(New) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
  }
}

object ClassSyntax extends SyntaxChecking.SyntaxChecker(ClassDec) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]){
    if (lits.exists(l => !l.isInstanceOf[Node] || !(l.asInstanceOf[Node].kind == FieldDec)))
      error(s"Expected field declarations but got ${lits.filter(l => !l.isInstanceOf[Node] || !l.asInstanceOf[Node].kind.isInstanceOf[FieldDec.type])}")

    if (kids.exists(k => !(k.kind == MethodDec)))
      error(s"Expected method declarations but got ${kids.filter(k => !(k.kind == MethodDec))}")
  }
}

object FieldSyntax extends SyntaxChecking.SyntaxChecker(FieldDec) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]){
    if (kids.nonEmpty)
      error(s"Expected no kids but got $kids")

    for (i <- 0 until lits.size by 2) {
      val name = lits(i)
      if (i + 1 >= lits.size)
        error(s"Field $name misses annotated type")
      val typ = lits(i+1)
      if (!name.isInstanceOf[Symbol])
        error(s"Expected field name of type Symbol but got $name")
      if (!typ.isInstanceOf[Type])
        error(s"Expected field type of type Type but got $typ")
    }
  }
}

object MethodSyntax extends SyntaxChecking.SyntaxChecker(MethodDec) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]){
   // if (lits.exists(l => !l.isInstanceOf[Node] || !(l.asInstanceOf[Node].kind == FieldDec)))
     // error(s"Expected field declarations but got ${lits.filter(l => !l.isInstanceOf[Node] || !l.asInstanceOf[Node].kind.isInstanceOf[FieldDec.type])}")


  }
}
