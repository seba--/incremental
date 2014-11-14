package incremental

abstract class ExpKind(val arity: Int)

import Exp._
case class Exp_[T](kind: ExpKind, lits: Seq[Lit], kids: Seq[Exp_[T]]) {
  var availableKidTypes: Seq[Boolean] = kids map (_.typ != null)
  var parent: Exp_[T] = _
  var pos: Int = _
  var typ: T = _

  for (i <- 0 until kids.size) {
    kids(i).parent = this
    kids(i).pos = i
  }

  def withType[T] = this.asInstanceOf[Exp_[T]]

  def markKidTypeAvailable(pos: Int) =
    availableKidTypes = availableKidTypes.updated(pos, true)

  def allKidTypesAvailable = availableKidTypes.foldLeft(true)(_&&_)

  def leaves: Seq[Exp_[T]] = {
    val buf = collection.mutable.ArrayBuffer[Exp_[T]]()
    leaves(buf)
    buf
  }

  def leaves(buf: collection.mutable.ArrayBuffer[Exp_[T]]): Unit = {
    if (kids.isEmpty)
      buf += this
    else
      kids foreach (_.leaves(buf))
  }

  override def toString = {
    val subs = lits.map(_.toString) ++ kids.map(_.toString)
    val subssep = if (subs.isEmpty) subs else subs.flatMap(s => Seq(", ", s)).tail
    val substring = subssep.foldLeft("")(_+_)
    s"$kind($substring)"
  }
}

object Exp {
  type Lit = Any
  type Exp = Exp_[Nothing]

  import scala.language.implicitConversions
  implicit def constructable(k: ExpKind) = new Constructable(k)
  class Constructable(k: ExpKind) {
    def apply(): Exp = Exp_[Nothing](k, Seq(), Seq())
    def apply(l: Lit, sub: Exp*): Exp = Exp_[Nothing](k, scala.Seq(l), Seq(sub:_*))
    def apply(e: Exp, sub: Exp*): Exp = Exp_[Nothing](k, scala.Seq(), e +: Seq(sub:_*))
    def apply(lits: Seq[Lit], sub: Seq[Exp]): Exp = Exp_[Nothing](k, lits, sub)
  }
}