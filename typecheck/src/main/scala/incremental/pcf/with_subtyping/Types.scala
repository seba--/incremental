package incremental.pcf.with_subtyping

import incremental.{TypCompanion, SType}


/**
 * Created by oliver on 19.11.14.
 */
trait Type extends SType[Type]
object Type {
  implicit object Companion extends TypCompanion[Type]
}

import Type.Companion._

case object Top extends Type {
  val isGround = true
  def freeTVars = Set()
  def occurs(x: Symbol) = false

  def subst(s: TSubst) = this
}

case object TNum extends Type {
  val isGround = true
  def freeTVars = Set()
  def occurs(x: Symbol) = false
  def subst(s: TSubst) = this
}

case class TVarInternal(x: Symbol) extends Type {
  val isGround = false
  def freeTVars = Set()
  def occurs(x2: Symbol) = x == x2
  def subst(s: TSubst) = s.getOrElse(x, this)
}

case class TFun(t1: Type, t2: Type) extends Type {
  val isGround = t1.isGround && t2.isGround
  def freeTVars = t1.freeTVars ++ t2.freeTVars
  def occurs(x: Symbol) = t1.occurs(x) || t2.occurs(x)
  def subst(s: TSubst) = TFun(t1.subst(s), t2.subst(s))
  override def toString= s"($t1 --> $t2)"
}

case object TNumeric extends Type {
  val isGround = true
  def freeTVars = Set()
  def occurs(x: Symbol) = false
  def subst(s: Map[Symbol, Type]) = this
}

case object TFloat extends Type {
  val isGround = true
  def freeTVars = Set()
  def occurs(x: Symbol) = false
  def subst(s: Map[Symbol, Type]) = this
}