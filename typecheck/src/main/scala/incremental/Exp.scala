package incremental

abstract class ExpKind(val arity: Int)

import Exp._
class Exp_[T](val kind: ExpKind, val lits: Seq[Lit], kidsArg: Seq[Exp_[T]]) {
  var parent: Exp_[T] = _
  var pos: Int = _
  var typ: T = _

  private val _kids: collection.mutable.ArrayBuffer[Exp_[T]] = collection.mutable.ArrayBuffer() ++= kidsArg
  private var availableKidTypes: Seq[Boolean] = kidsArg map (_.typ != null)

  def kids = new Object {
    def apply(i: Int) = _kids(i)
    def update(i: Int, e: Exp_[T]): Unit = {
      _kids(i) = e
      typ = null.asInstanceOf[T]
    }
  }

  for (i <- 0 until _kids.size) {
    _kids(i).parent = this
    _kids(i).pos = i
  }

  def withType[T] = this.asInstanceOf[Exp_[T]]

  def markKidTypeAvailable(pos: Int) =
    availableKidTypes = availableKidTypes.updated(pos, true)

  def allKidTypesAvailable = availableKidTypes.foldLeft(true)(_&&_)

  def uninitialized: Seq[Exp_[T]] = {
    val buf = collection.mutable.ArrayBuffer[Exp_[T]]()
    uninitialized(buf)
    buf
  }

  def uninitialized(buf: collection.mutable.ArrayBuffer[Exp_[T]]): Unit = {
    _kids foreach (_.uninitialized(buf))
    if (typ == null)
      buf += this
  }

  override def toString = {
    val subs = lits.map(_.toString) ++ _kids.map(_.toString)
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
    def apply(): Exp = new Exp_[Nothing](k, Seq(), Seq())
    def apply(l: Lit, sub: Exp*): Exp = new Exp_[Nothing](k, scala.Seq(l), Seq(sub:_*))
    def apply(e: Exp, sub: Exp*): Exp = new Exp_[Nothing](k, scala.Seq(), e +: Seq(sub:_*))
    def apply(lits: Seq[Lit], sub: Seq[Exp]): Exp = new Exp_[Nothing](k, lits, sub)
  }
}