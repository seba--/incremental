package incremental.pcf

import constraints.equality.Type
import incremental.Node_

trait Exp_[T] extends Node_[T]
object Exp {
  type Exp = Exp_[Any]
}

case class Num[T](n: Int) extends Exp_[T] {
  def foldKids[R](init: R)(f: (R, Node_[T]) => R) = init
  def updateKid(i: Int, n: Node_[T]) = throw new IndexOutOfBoundsException
}
case class Add[T](private var e1: Exp_[T], private var e2: Exp_[T]) extends Exp_[T] {
  def foldKids[R](init: R)(f: (R, Node_[T]) => R) = f(f(init, e1), e2)
  def updateKid(i: Int, n: Node_[T]) = i match {
    case 0 => e1 = n.asInstanceOf[Exp_[T]]
    case 1 => e2 = n.asInstanceOf[Exp_[T]]
  }
}
case class Mul[T](private var e1: Exp_[T], private var e2: Exp_[T]) extends Exp_[T] {
  def foldKids[R](init: R)(f: (R, Node_[T]) => R) = f(f(init, e1), e2)
  def updateKid(i: Int, n: Node_[T]) = i match {
    case 0 => e1 = n.asInstanceOf[Exp_[T]]
    case 1 => e2 = n.asInstanceOf[Exp_[T]]
  }
}
case class Var[T](x: Symbol) extends Exp_[T] {
  def foldKids[R](init: R)(f: (R, Node_[T]) => R) = init
  def updateKid(i: Int, n: Node_[T]) = throw new IndexOutOfBoundsException
}
case class Abs[T](x: Symbol, t: Option[Type], private var e: Exp_[T]) extends Exp_[T] {
  def foldKids[R](init: R)(f: (R, Node_[T]) => R) = f(init, e)
  def updateKid(i: Int, n: Node_[T]) = i match {
    case 0 => e = n.asInstanceOf[Exp_[T]]
  }
}
object Abs {
  def apply[T](x: Symbol, e: Exp_[T]): Abs[T] = Abs[T](x, None, e)
  def apply[T](x: Symbol, t: Type, e: Exp_[T]): Abs[T] = Abs[T](x, Some(t), e)
}
case class AbsMany[T](xs: Seq[Symbol], private var e: Exp_[T]) extends Exp_[T] {
  def foldKids[R](init: R)(f: (R, Node_[T]) => R) = f(init, e)
  def updateKid(i: Int, n: Node_[T]) = i match {
    case 0 => e = n.asInstanceOf[Exp_[T]]
  }
}
case class App[T](private var e1: Exp_[T], private var e2: Exp_[T]) extends Exp_[T] {
  def foldKids[R](init: R)(f: (R, Node_[T]) => R) = f(f(init, e1), e2)
  def updateKid(i: Int, n: Node_[T]) = i match {
    case 0 => e1 = n.asInstanceOf[Exp_[T]]
    case 1 => e2 = n.asInstanceOf[Exp_[T]]
  }
}
case class If0[T](private var c: Exp_[T], private var t: Exp_[T], private var e: Exp_[T]) extends Exp_[T] {
  def foldKids[R](init: R)(f: (R, Node_[T]) => R) = f(f(f(init, c), t), e)
  def updateKid(i: Int, n: Node_[T]) = i match {
    case 0 => c = n.asInstanceOf[Exp_[T]]
    case 1 => t = n.asInstanceOf[Exp_[T]]
    case 2 => e = n.asInstanceOf[Exp_[T]]
  }
}
case class Fix[T](private var e: Exp_[T]) extends Exp_[T] {
  def foldKids[R](init: R)(f: (R, Node_[T]) => R) = f(init, e)
  def updateKid(i: Int, n: Node_[T]) = i match {
    case 0 => e = n.asInstanceOf[Exp_[T]]
  }
}
