package incremental.fjava.earlymerge

import constraints.CVar
import constraints.fjava.CSubst.CSubst
import constraints.fjava.{Constraint, ConstraintSystem, Type}
import incremental.fjava.{CName, UCName}
import incremental.IncHashedSet

object Condition {
  val trueCond = new Condition(IncHashedSet.empty, IncHashedSet.empty, IncHashedSet.empty, IncHashedSet.empty) {
    override def subst(cls: Type, s: CSubst): Option[Condition] = Some(this)
    override def toString: String = "trueCond"

    override def equals(obj: Any): Boolean = obj.isInstanceOf[AnyRef] && eq(obj.asInstanceOf[AnyRef])

    override lazy val hashCode: Int = 0
  }

  def apply(notGround: IncHashedSet[CName], notVar: IncHashedSet[UCName], sameGroundAlternatives: IncHashedSet[CName], sameVar: IncHashedSet[UCName]): Condition =
    if (notGround.isEmpty && notVar.isEmpty && sameGroundAlternatives.isEmpty && sameVar.isEmpty)
      trueCond
    else
      new Condition(notGround, notVar, sameGroundAlternatives, sameVar)
}

trait ConditionTrait {
  def subst(cls: Type, s: CSubst): Option[ConditionTrait]
  def uvars: Set[CVar[Type]]

  def mergeKey: MergeKey
}

class Condition(
         val notGround: IncHashedSet[CName],
         val notVar: IncHashedSet[UCName],
         val sameGroundAlternatives: IncHashedSet[CName], // empty = no requirement, non-empty = cls must be one of the elements
         val sameVar: IncHashedSet[UCName]) extends ConditionTrait {

  //  if (sameGround.exists(notGround.contains(_)))
  //    println("WARNING 1")
  //  if (sameGround.size > 1)
  //    println("WARNING 2")
  //  if (sameGround.nonEmpty && notGround.nonEmpty)
  //    println("WARNING 4")
  //  if (othersGround.values.exists(trueCond == _))
  //    println("WARNING 5")
  //  if (othersVar.values.exists(trueCond == _))
  //    println("WARNING 6")
  //  if (this == trueCond)
  //    println("WARNING 7")

//  if (sameGroundAlternatives.size > 1)
//    println(s"Interesting same size $sameGroundAlternatives")

  final def mergeKey = new ConditionMergeKey(this)

  def subst(cls: Type, s: CSubst): Option[Condition] = {

    var newsameGroundAlternatives: IncHashedSet[CName] =
      if (!cls.isGround)
        sameGroundAlternatives
      else if (sameGroundAlternatives.isEmpty || sameGroundAlternatives.contains(cls.asInstanceOf[CName]))
        IncHashedSet.empty[CName]
      else
        return None

    val newsameVar: IncHashedSet[UCName] = sameVar flatMap { vt =>
      val n2 = vt.subst(s)
      if (cls == n2)
        None
      else if (n2.isGround) {
        if (cls.isGround) // && cls != n2 (implicit)
          return None
        else {
          val ct = n2.asInstanceOf[CName]
          if (!newsameGroundAlternatives.isEmpty) {
            if (newsameGroundAlternatives.contains(ct))
              None
            else
              return None
          }
          else {
            newsameGroundAlternatives = IncHashedSet(ct)
            None
          }
        }
      }
      else
        Some(n2.asInstanceOf[UCName])
    }

    var newnotGround: IncHashedSet[CName] = notGround flatMap { ct =>
      if (cls == ct)
        return None
      else if (cls.isGround) // && cls != n2 (implicit)
        None
      else if (!newsameGroundAlternatives.isEmpty) {
        if (newsameGroundAlternatives.contains(ct))
          None
        else
          return None
      }
      else
        Some(ct)
    }

    val newnotVar: IncHashedSet[UCName] = notVar flatMap { vt =>
      val n2 = vt.subst(s)
      if (cls == n2)
        return None
      else if (n2.isGround) {
        if (cls.isGround) // && cls != n2 (implicit)
          None
        else {
          val ct = n2.asInstanceOf[CName]
          newnotGround += ct
          None
        }
      }
      else
        Some(n2.asInstanceOf[UCName])
    }

    Some(Condition(newnotGround, newnotVar, newsameGroundAlternatives, newsameVar))
  }

  final def alsoNot(cls: Type, ct: CName): Option[Condition] =
    if (ct == cls)
      None
    else if (cls.isGround) // because cls != t
      Some(this)
    else if (!sameGroundAlternatives.isEmpty) {
      if (sameGroundAlternatives.contains(ct))
        None
      else
        return None
    }
    else
      Some(new Condition(notGround + ct, notVar, sameGroundAlternatives, sameVar))

  final def alsoNot(cls: Type, vt: UCName): Option[Condition] =
    if (vt == cls)
      None
    else if (sameVar.contains(vt))
      None
    else
      Some(new Condition(notGround, notVar + vt, sameGroundAlternatives, sameVar))

  final def alsoSame(cls: Type, ct: CName): Option[Condition] =
    if (ct == cls)
      Some(this)
    else if (cls.isGround || notGround.contains(ct))  // because cls != ct
      None
    else if (!sameGroundAlternatives.isEmpty) {
      if (sameGroundAlternatives.contains(ct))
        Some(this)
      else
        None
    }
    else
      Some(new Condition(IncHashedSet.empty, notVar, IncHashedSet(ct), sameVar))

  final def alsoSame(cls: Type, vt: UCName): Option[Condition] =
    if (vt == cls)
      Some(this)
    else if (notVar.contains(vt)) // because cls != vt
      None
    else
      Some(new Condition(notGround, notVar, sameGroundAlternatives, sameVar + vt))

  final def forCls(ct: CName): Option[Condition] =
    if (!sameGroundAlternatives.isEmpty) {
      if (sameGroundAlternatives.contains(ct))
        Some(Condition(notGround, notVar, IncHashedSet.empty, sameVar))
      else
        None
    }
    else if (notGround.contains(ct))
      None
    else
      Some(Condition(IncHashedSet.empty, notVar, sameGroundAlternatives, sameVar))

    final def alsoCond(cls: Type, other: Condition): Option[Condition] = {
      var res: Option[Condition] = Some(this)
      other.notGround.foreach{ c => res = res match {case None => None; case Some(cond) => cond.alsoNot(cls, c) }}
      other.notVar.foreach{ c => res = res match {case None => None; case Some(cond) => cond.alsoNot(cls, c) }}
//      println(s"ConditionNested.alsoCond other.sameGroundAlternatives ${other.sameGroundAlternatives}")
      other.sameGroundAlternatives.foreach{ c => res = res match {case None => None; case Some(cond) => cond.alsoSame(cls, c) }}
      other.sameVar.foreach{ c => res = res match {case None => None; case Some(cond) => cond.alsoSame(cls, c) }}
      res
    }


  def uvars: Set[CVar[Type]] = notVar.set.map(_.x) ++ sameVar.set.map(_.x)

  override def toString: String = {
    val not = notGround.asInstanceOf[IncHashedSet[Type]] ++ notVar
    val same = sameGroundAlternatives.asInstanceOf[IncHashedSet[Type]] ++ sameVar
    s"diff(${not.mkString(", ")}),same(${same.mkString(", ")})"
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Condition =>
      notGround == other.notGround && notVar == other.notVar && sameGroundAlternatives == other.sameGroundAlternatives && sameVar == other.sameVar
    case _ => false
  }

  override lazy val hashCode: Int = notGround.hashCode() + 31*notVar.hashCode() + 61*sameGroundAlternatives.hashCode() + 97*sameVar.hashCode()
}

object ConditionOther {
  def apply(cls: CName, cond: Condition): ConditionTrait =
    if (Condition.trueCond == cond)
      Condition.trueCond
    else
      new ConditionOther(cls, cond)
}

final class ConditionOther(val cls: CName, val cond: Condition) extends ConditionTrait {

  override def toString: String = s"others($cls -> $cond)"

  override def subst(clsx: Type, s: CSubst): Option[ConditionTrait] =
    cond.subst(cls, s) match {
      case None => return None
      case Some(c) => Some(ConditionOther(cls, c))
    }

  override def uvars: Set[CVar[Type]] = cond.uvars

  override def mergeKey: ConditionOtherMergeKey = new ConditionOtherMergeKey(this)

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: ConditionOther => cls == other.cls && cond == other.cond
    case _ => false
  }

  override def hashCode: Int = cls.hashCode + 31*cond.hashCode
}


trait MergeKey {
  def needsMerge(c2: ConditionTrait): Boolean
}

class ConditionOtherMergeKey(val c: ConditionOther) extends MergeKey {

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: ConditionOtherMergeKey =>
      c.cls == other.c.cls &&
      c.cond.notGround == other.c.cond.notGround &&
      c.cond.notVar == other.c.cond.notVar &&
      // ignore sameGroundAlternatives
      c.cond.sameVar == other.c.cond.sameVar
    case _ => false
  }

  override val hashCode: Int = c.cls.hashCode + 31*c.cond.notGround.hashCode + 67*c.cond.notVar.hashCode + 97*c.cond.sameVar.hashCode


  def needsMerge(c2: ConditionTrait): Boolean = {
    val other = c2.asInstanceOf[ConditionOther]
    c.cond.sameGroundAlternatives != other.cond.sameGroundAlternatives
  }
}

class ConditionMergeKey(val c: Condition) extends MergeKey {
  override def equals(obj: Any): Boolean = obj match {
    case otherMerge: ConditionMergeKey =>
      val other = otherMerge.c

      c.notGround == other.notGround &&
      c.notVar == other.notVar &&
      // ignore sameGroundAlternatives
      c.sameVar == other.sameVar
    case _ => false
  }

  override val hashCode: Int = {
    c.notGround.hashCode() +
      31*c.notVar.hashCode() +
      // ignore sameGroundAlternatives
      97*c.sameVar.hashCode()
  }

  def needsMerge(c2: ConditionTrait): Boolean =
    c.sameGroundAlternatives != c2.asInstanceOf[Condition].sameGroundAlternatives
}




class Conditional(val cons: Constraint, val cls: Type, val cond: ConditionTrait) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = {
    val cls_ = cls.subst(cs.substitution)
    cond.subst(cls_, cs.substitution) match {
      case None => cs // discard this constraint because condition is false
      case Some(cond_) if Condition.trueCond == cond_ => cons.solve(cs)
      case Some(cond_) => cs.notyet(new Conditional(cons.subst(cs.substitution), cls_, cond_))
    }
  }

  override def subst(s: CSubst): Constraint = {
    val cls_ = cls.subst(s)
    Conditional(cons.subst(s), cls_, cond.subst(cls_, s).getOrElse(Condition.trueCond))
  }

  override def uvars: Set[CVar[Type]] = cons.uvars ++ cls.uvars ++ cond.uvars

  override def toString: String = s"Conditional($cons, $cls, $cond)"

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Conditional =>
      (cons == other.cons) && (cls == other.cls) && (cond == other.cond)
    case _ => false
  }

  override lazy val hashCode: Int = cons.hashCode + 31*cls.hashCode + 61*cond.hashCode
}
object Conditional {
  def apply(cons: Constraint, cls: Type, cond: ConditionTrait): Constraint =
    if (cond == Condition.trueCond)
      cons
    else
      new Conditional(cons, cls, cond)
}

