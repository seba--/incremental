package incremental

import Exp._
abstract class ExpKind(val arity: Int) {
  def unapplySeq(e: Exp_[_]): Option[Seq[Exp_[_]]] =
    if (e.kind == this)
      Some(e.kids.seq)
    else
      None
}

class Exp_[T](val kind: ExpKind, val lits: Seq[Lit], kidsArg: Seq[Exp_[T]]) {
  private var _typ: T = _
  private var _valid = false

  def valid = _valid
  def typ = _typ
  def typ_=(t: T): Unit = {
    _typ = t
    _valid = true
  }

  private val _kids: Array[Exp_[T]] = Array(kidsArg:_*)
  private var availableKidTypes: Seq[Boolean] = kidsArg map (_.typ != null)

  object kids {
    def apply(i: Int) = _kids(i)
    def update[U](i: Int, e: Exp_[U]): Unit = {
      val ee = e.asInstanceOf[Exp_[T]]
      if (ee._valid)
        _valid = false
      else
        ee._typ = kids(i)._typ
      _kids(i) = ee
    }
    def seq: Seq[Exp_[T]] = _kids
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
    val oldsize = buf.size
    _kids foreach (_.uninitialized(buf))
    val hasSubchange = oldsize == buf.size
    if (!valid || hasSubchange)
      buf += this
  }

  def visitUninitialized(f: Exp_[T] => Boolean): Boolean = {
    val hasSubchange = _kids.foldLeft(false)((changed, k) =>  k.visitUninitialized(f) || changed)
    if (!valid || hasSubchange)
      f(this)
    else
      false
  }

  def visitUninitialized2(f: Exp_[T] => (T, Boolean)): Boolean = {
    val hasSubchange = _kids.foldLeft(false)((changed, k) =>  k.visitUninitialized2(f) || changed)
    if (!valid || hasSubchange) {
      val (t, change) = f(this)
      _typ = t
      _valid = true
      change
    }
    else
      false
  }

  override def toString = {
    val subs = lits.map(_.toString) ++ _kids.map(_.toString)
    val subssep = if (subs.isEmpty) subs else subs.flatMap(s => Seq(", ", s)).tail
    val substring = subssep.foldLeft("")(_+_)
    val typString = "" //if(typ == null) "" else "@{" + typ.asInstanceOf[Tuple3[_,_,_]]._1 "}"
    s"$kind$typString($substring)"
  }
}

object Exp {
  type Lit = Any
  type Exp = Exp_[Nothing]

  import scala.language.implicitConversions
  implicit def kindExpression(k: ExpKind) = new KindExpression(k)
  class KindExpression(k: ExpKind) {
    def apply(): Exp = new Exp_[Nothing](k, Seq(), Seq())
    def apply(l: Lit, sub: Exp*): Exp = new Exp_[Nothing](k, scala.Seq(l), Seq(sub:_*))
    def apply(l1: Lit, l2: Lit, sub: Exp*): Exp = new Exp_[Nothing](k, scala.Seq(l1, l2), Seq(sub:_*))
    def apply(e: Exp, sub: Exp*): Exp = new Exp_[Nothing](k, scala.Seq(), e +: Seq(sub:_*))
    def apply(lits: Seq[Lit], sub: Seq[Exp]): Exp = new Exp_[Nothing](k, lits, sub)
  }
}