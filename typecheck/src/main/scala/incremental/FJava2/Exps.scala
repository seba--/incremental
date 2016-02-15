package incremental.FJava2

import constraints.fjava.Type
import incremental.{NodeKind, SyntaxChecking}
import incremental.Node._
import incremental.{Node_, SyntaxChecking}

import scala.collection.immutable.ListMap


abstract class Toplevel(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)

case object ProgramM extends NodeKind(_ => ProgramSyntax)

case object ClassDec extends Toplevel(_ => ClassSyntax)
case class Ctor(superParams: ListMap[Symbol, CName], fields: ListMap[Symbol, CName])

case object FieldDec extends Toplevel(_ => FieldSyntax)
case object MethodDec extends Toplevel(_ => MethodSyntax)

abstract class Exp(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Exp {
  val cExp = classOf[Exp]
}
import Exp._

case object Num extends Exp(simple(Seq(classOf[Integer])))
case object Str extends Exp(simple(Seq(classOf[Symbol])))
case object Add extends Exp(simple(cExp, cExp))
case object Mul extends Exp(simple(cExp, cExp))
case object Var  extends Exp(simple(Seq(classOf[Symbol])))
case object This extends Exp(simple(Seq(classOf[Symbol])))
case object FieldAcc extends Exp(simple(Seq(classOf[Symbol]), cExp))
case object New extends Exp(_ => NewSyntax)
case object UCast extends Exp(simple(Seq(classOf[CName]),cExp))
case object Invk extends Exp(_ =>InvkSyntax)
case object TClass extends Exp(simple(cExp))
case object DCast extends Exp(simple(cExp))
case object SCast extends Exp(simple(cExp))

object InvkSyntax extends SyntaxChecking.SyntaxChecker(Invk) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if ((lits.size > 1) && (!(lits(0).isInstanceOf[Symbol])))
      error(s"Method name should be a symbol, but found ${(!(lits(0).isInstanceOf[Symbol]))}")
    if (kids.exists(!_.kind.isInstanceOf[Exp]))
      error(s"All kids must be of sort Exp, but found ${kids.filter(!_.kind.isInstanceOf[Exp])}")

  }
}

object NewSyntax extends SyntaxChecking.SyntaxChecker(New) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if (lits.size != 1)
      error(s"Wrong number of literals. Expected one literal (class name), got $lits")
    if (!lits(0).isInstanceOf[CName])
      error(s"Class name should be a CName, but found ${(!(lits(0).isInstanceOf[CName]))}")
    if (kids.exists(!_.kind.isInstanceOf[Exp]))
      error(s"All kids must be of sort Exp, but found ${kids.filter(!_.kind.isInstanceOf[Exp])}")

  }
}

object ClassSyntax extends SyntaxChecking.SyntaxChecker(ClassDec) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]){

    if (!(lits(0).isInstanceOf[CName]))
      error(s"Expected Class type CName, but got ${lits(0)}")

    if (!(lits(1).isInstanceOf[CName]))
      error(s"Expected Super type CName, but got ${lits(1)}")

    //if ((lits.size > 3) && !(lits(2) == CtorDec))
   // if  (!(lits(2) == CtorDec))
     // error(s"Expected Ctor spec, but got ${lits(2)}")
    //TODO syntax check Ctor

    for (i <- 3 until lits.size - 3 by 2) {
      val name = lits(i)
      if (i + 1 >= lits.size)
        error(s"Field $name misses annotated type")
      val typ = lits(i+1)
      if (!name.isInstanceOf[Symbol])
        error(s"Expected field name of type Symbol but got $name")
      if (!typ.isInstanceOf[Type])
        error(s"Expected field type of type Type but got $typ")
    }

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

    if (lits.size != 3)
      error(s"Wrong number of arguments. Expected Method(return, name, params), but got $lits")

    if (!(lits(0).isInstanceOf[CName]))
      error(s"Expected return type CName, but got ${lits(3)}")
//      error(s"Expected method owner CName, but got ${lits(0)}")

    if (!(lits(1).isInstanceOf[Symbol]))
      error(s"Expected Method name Symbol, but got ${lits(1)}")

    if (!lits(2).isInstanceOf[Seq[_]])
      error(s"Expected parameter list, but got ${lits(2)}")
    val params = lits(2).asInstanceOf[Seq[_]]
    for (i <- 0 until params.size by 2) {
      val nametype = params(i)
      if (!nametype.isInstanceOf[(_, _)])
        error(s"Expected name/type pair, but got $nametype")
      val name = nametype.asInstanceOf[(_,_)]._1
      val typ = nametype.asInstanceOf[(_,_)]._2
      if (!name.isInstanceOf[Symbol])
        error(s"Expected field name of type Symbol but got $name")
      if (!typ.isInstanceOf[Type])
        error(s"Expected field type of type Type but got $typ")
    }

//    if (!(lits(3).isInstanceOf[CName]))
//      error(s"Expected return type CName, but got ${lits(3)}")

    if (kids.exists(!_.kind.isInstanceOf[Exp]))
      error(s"All kids must be of sort Exp, but found ${kids.filter(!_.kind.isInstanceOf[Exp])}")
  }
}

object ProgramSyntax extends SyntaxChecking.SyntaxChecker(ProgramM) {
  def check[T](lits: Seq[Lit] , kids: Seq[Node_[T]]){
    if (lits.nonEmpty)
      error(s"No literals expected")

    if (kids.exists(_.kind != ClassDec))
      error(s"All children of a program must be class declarations, but found ${kids.filter(_.kind != ClassDec)}")
  }
}

object ThisSyntax extends SyntaxChecking.SyntaxChecker(Invk) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
  }
}


