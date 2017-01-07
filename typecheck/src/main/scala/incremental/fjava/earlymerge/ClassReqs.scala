package incremental.fjava.earlymerge

import constraints.fjava.CSubst.CSubst
import constraints.fjava._
import Condition.trueCond
import constraints.CVar
import incremental.Util
import incremental.fjava.{CName, UCName}

import scala.collection.generic.CanBuildFrom

trait CReq[T <: CReq[T]] {
  def self: T
  val cls: Type
  def withCls(t: Type): T = {
    val newcond = Condition(cond.notGround, cond.notVar, cond.sameGround, cond.sameVar, Map(), Map())
    if (trueCond == newcond)
      withCls(t, Condition(Set(), Set(), None, Set(), cond.othersGround, cond.othersVar))
    else if (cls.isGround)
      withCls(t, Condition(Set(), Set(), None, Set(), cond.othersGround + (cls.asInstanceOf[CName] -> newcond), cond.othersVar))
    else
      withCls(t, Condition(Set(), Set(), None, Set(), cond.othersGround, cond.othersVar + (cls.asInstanceOf[UCName] -> newcond)))
  }
  def withCls(t: Type, newcond: Condition): T
  val cond: Condition
  def canMerge(other: CReq[T]): Boolean
  def assert(sat: CReq[T]): Option[Constraint] = alsoSame(sat.cls).map(c2 => this.assert(sat, c2.cond))
  def assert(other: CReq[T], cond: Condition): Constraint
  def subst(s: CSubst): Option[T]

  def withCond(cond: Condition): T
  def alsoNot(n: Type): Option[T] = cond.alsoNot(cls, n) map (withCond(_))
  def alsoSame(n: Type): Option[T] = cond.alsoSame(cls, n) map (withCond(_))

  def satisfyExt(cls: CName, sup: CName): Seq[T] = {
    // NOTE: we assume that the removal of an extends clause implies no further declarations appear for subclass `ext.cls`

    // ext.cls != req.cls, so keep the original requirement
    val diff = this.alsoNot(cls)
    // ext.cls == req.cls, so replace the original requirement with a super-class requirement `ext.ext`
    val same = this.alsoSame(cls).map(_.withCls(sup))

    if (diff.isEmpty) {
      if (same.isEmpty)
        Seq()
      else
        Seq(same.get)
    }
    else {
      if (same.isEmpty)
        Seq(diff.get)
      else
        Seq(diff.get, same.get)
    }
  }
}
case class ExtCReq(cls: Type, ext: Type, cond: Condition = trueCond) extends CReq[ExtCReq] {
  def self = this
  def withCls(t: Type, newcond: Condition) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[ExtCReq]) = true
  def assert(other: CReq[ExtCReq], cond: Condition) = Conditional(Equal(ext, other.self.ext), cls, cond)
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (ExtCReq(cls_, ext.subst(s), _))
  }
  def withCond(c: Condition) = copy(cond = c)
}
case class CtorCReq(cls: Type, args: Seq[Type], cond: Condition = trueCond) extends CReq[CtorCReq] {
  def self = this
  def withCls(t: Type, newcond: Condition) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[CtorCReq]) = true
  def assert(other: CReq[CtorCReq], cond: Condition) = Conditional(AllEqual(args, other.self.args), cls, cond)
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (CtorCReq(cls_, args.map(_.subst(s)), _))
  }
  def withCond(c: Condition) = copy(cond = c)
}
trait Named {
  val name: Symbol
}
case class FieldCReq(cls: Type, name: Symbol, typ: Type, cond: Condition = trueCond) extends CReq[FieldCReq] with Named {
  def self = this
  def withCls(t: Type, newcond: Condition) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[FieldCReq]): Boolean = name == other.self.name
  def assert(other: CReq[FieldCReq], cond: Condition) = Conditional(Equal(typ, other.self.typ), cls, cond)
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (FieldCReq(cls_, name, typ.subst(s), _))
  }
  def withCond(c: Condition) = copy(cond = c)
}
case class MethodCReq(cls: Type, name: Symbol, params: Seq[Type], ret: Type, optionallyDefined: Boolean = false, cond: Condition = trueCond) extends CReq[MethodCReq] with Named {
  def self = this
  def withCls(t: Type, newcond: Condition) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[MethodCReq]): Boolean = name == other.self.name
  def assert(other: CReq[MethodCReq], cond: Condition) = Conditional(AllEqual(ret +: params, other.self.ret +: other.self.params), cls, cond)
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (MethodCReq(cls_, name, params.map(_.subst(s)), ret.subst(s), optionallyDefined, _))
  }
  def uvars = cls.uvars ++ params.flatMap(_.uvars) ++ cond.uvars
  def withCond(c: Condition) = copy(cond = c)
}

object Condition {
  val trueCond = new Condition(Set(), Set(), None, Set(), Map(), Map()) {
    override def subst(cls: Type, s: CSubst): Option[Condition] = Some(this)
    override def toString: String = "trueCond"

    override def equals(obj: Any): Boolean = obj.isInstanceOf[AnyRef] && eq(obj.asInstanceOf[AnyRef])

    override def hashCode: Int = 0

    override def alsoCond(cls: Type, other: Condition) = Some(other)
  }

  def apply(notGround: Set[CName], notVar: Set[UCName], sameGround: Option[CName], sameVar: Set[UCName], othersGround: Map[CName, Condition], othersVar: Map[UCName, Condition]): Condition =
    if (notGround.isEmpty && notVar.isEmpty && sameGround.isEmpty && sameVar.isEmpty && othersGround.isEmpty && othersVar.isEmpty)
      trueCond
    else
      new Condition(notGround, notVar, sameGround, sameVar, othersGround, othersVar)
}
class Condition(
       val notGround: Set[CName],
       val notVar: Set[UCName],
       val sameGround: Option[CName],
       val sameVar: Set[UCName],
       val othersGround: Map[CName, Condition],
       val othersVar: Map[UCName, Condition]) {

//  if (sameGround.exists(notGround.contains(_)))
//    println("WARNING 1")
//  if (sameGround.size > 1)
//    println("WARNING 2")
//  if (othersGround.size > 1)
//    println("WARNING 3")
//  if (sameGround.nonEmpty && notGround.nonEmpty)
//    println("WARNING 4")
//  if (othersGround.values.exists(trueCond == _))
//    println("WARNING 5")
//  if (othersVar.values.exists(trueCond == _))
//    println("WARNING 6")
//  if (this == trueCond)
//    println("WARNING 7")


  def subst(cls: Type, s: CSubst): Option[Condition] = {

    var newothersGround: Map[CName, Condition] = othersGround flatMap { kv =>
      val cls = kv._1
      kv._2.subst(cls, s) match {
        case None => return None
        case Some(`trueCond`) => None
        case Some(cond) => Some(cls -> cond)
      }
    }

    var newothersVar: Map[UCName, Condition] = othersVar flatMap { kv =>
      val cls = kv._1.subst(s)
      kv._2.subst(cls, s) match {
        case None => return None
        case Some(`trueCond`) => None
        case Some(cond) =>
          if (cls.isGround) {
            val ct = cls.asInstanceOf[CName]
            newothersGround.get(ct) match {
              case None =>
                newothersGround += (ct -> cond)
              case Some(cond1) =>
                newothersGround += (ct -> cond1.alsoCond(ct, cond).getOrElse(return None))
            }
            None
          }
          else {
            // TODO possible key collision `cls`?
            Some(cls.asInstanceOf[UCName] -> cond)
          }
      }
    }

    var newsameGround: Option[CName] = sameGround flatMap { ct =>
      if (cls == ct)
        None
      else if (cls.isGround) // && cls != n2 (implicit)
        return None
      else {
        newothersGround.get(ct) match {
          case None => Some(ct)
          case Some(cond) =>
            newothersGround += (ct -> cond.alsoSame(ct, cls).getOrElse(return None))
            None
        }
      }
    }

    val newsameVar: Set[UCName] = sameVar flatMap { vt =>
      val n2 = vt.subst(s)
      if (cls == n2)
        None
      else if (n2.isGround) {
        if (cls.isGround) // && cls != n2 (implicit)
          return None
        else {
          val ct = n2.asInstanceOf[CName]
          if (newsameGround.isDefined) {
            if (newsameGround.get != ct)
              return None
            else
              None
          }
          else {
            newothersGround.get(ct) match {
              case None =>
                newsameGround = Some(ct)
                None
              case Some(cond) =>
                newothersGround += (ct -> cond.alsoSame(ct, cls).getOrElse(return None))
                None
            }
          }
        }
      }
      else {
        val vt = n2.asInstanceOf[UCName]
        newothersVar.get(vt) match {
          case None =>
            Some(vt)
          case Some(cond) =>
            newothersVar += (vt -> cond.alsoSame(vt, cls).getOrElse(return None))
            None
        }
      }
    }

    var newnotGround: Set[CName] = notGround flatMap { ct =>
      if (cls == ct)
        return None
      else if (cls.isGround) // && cls != n2 (implicit)
        None
      else if (newsameGround.isDefined) {
        if (newsameGround.get != ct)
          return None
        else
          None
      }
      else newothersGround.get(ct) match {
        case None => Some(ct)
        case Some(cond) =>
          newothersGround += (ct -> cond.alsoNot(ct, cls).getOrElse(return None))
          None
      }
    }

    val newnotVar: Set[UCName] = notVar flatMap { vt =>
      val n2 = vt.subst(s)
      if (cls == n2)
        return None
      else if (n2.isGround) {
        if (cls.isGround) // && cls != n2 (implicit)
          None
        else {
          val ct = n2.asInstanceOf[CName]
          newothersGround.get(ct) match {
            case None =>
              newnotGround += ct
              None
            case Some(cond) =>
              newothersGround += (ct -> cond.alsoNot(ct, cls).getOrElse(return None))
              None
          }
        }
      }
      else {
        val vt = n2.asInstanceOf[UCName]
        newothersVar.get(vt) match {
          case None =>
            Some(vt)
          case Some(cond) =>
            newothersVar += (vt -> cond.alsoNot(vt, cls).getOrElse(return None))
            None
        }
      }
    }

    Some(Condition(newnotGround, newnotVar, newsameGround, newsameVar, newothersGround, newothersVar))
  }

  def alsoNot(cls: Type, t: Type): Option[Condition] = {
    if (cls == t)
      None
    else if (t.isGround) {
      val ct = t.asInstanceOf[CName]
      if (cls.isGround) // because cls != t
        Some(this)
      else if (sameGround.isDefined) {
        if (sameGround.get != ct)
          return None
        else
          None
      }
      else othersGround.get(ct) match {
        case None => Some(new Condition(notGround + ct, notVar, sameGround, sameVar, othersGround, othersVar))
        case Some(c) => c.alsoNot(ct, cls) match {
          case None => None
          case Some(`trueCond`) => Some(Condition(notGround, notVar, sameGround, sameVar, othersGround - ct, othersVar))
          case Some(c_) => Some(new Condition(notGround, notVar, sameGround, sameVar, othersGround + (ct -> c_), othersVar))
        }
      }
    }
    else {
      val vt = t.asInstanceOf[UCName]
      if (sameVar.contains(vt))
        None
      else othersVar.get(vt) match {
        case None => Some(new Condition(notGround, notVar + vt, sameGround, sameVar, othersGround, othersVar))
        case Some(c) => c.alsoNot(vt, cls) match {
          case None => None
          case Some(`trueCond`) => Some(Condition(notGround, notVar, sameGround, sameVar, othersGround, othersVar - vt))
          case Some(c_) => Some(new Condition(notGround, notVar, sameGround, sameVar, othersGround, othersVar + (vt -> c_)))
        }
      }
    }
  }

  def alsoSame(cls: Type, t: Type): Option[Condition] =
    if (cls == t)
      Some(this)
    else if (t.isGround) {
      val ct = t.asInstanceOf[CName]
      if (sameGround.isDefined) {
        if (ct != sameGround.get)
          None
        else
          Some(this)
      }
      else if (cls.isGround || notGround.contains(ct)) // because cls != t
        None
      else othersGround.get(ct) match {
        case None => Some(new Condition(Set(), notVar, Some(ct), sameVar, othersGround, othersVar))
        case Some(c) => c.alsoNot(ct, cls) match {
          case None => None
          case Some(`trueCond`) => Some(Condition(notGround, notVar, sameGround, sameVar, othersGround - ct, othersVar))
          case Some(c_) => Some(new Condition(notGround, notVar, sameGround, sameVar, othersGround + (ct -> c_), othersVar))
        }
      }
    }
    else {
      val vt = t.asInstanceOf[UCName]
      if (notVar.contains(vt)) // because cls != t
        None
      else othersVar.get(vt) match {
        case None => Some(new Condition(notGround, notVar, sameGround, sameVar + vt, othersGround, othersVar))
        case Some(c) => c.alsoNot(vt, cls) match {
          case None => None
          case Some(`trueCond`) => Some(Condition(notGround, notVar, sameGround, sameVar, othersGround, othersVar - vt))
          case Some(c_) => Some(new Condition(notGround, notVar, sameGround, sameVar, othersGround, othersVar + (vt -> c_)))
        }
      }
    }

  def alsoCond(cls: Type, other: Condition): Option[Condition] =
    if (cls.isGround && other.notGround.contains(cls.asInstanceOf[CName]))
      None
    else if (other.notGround.exists(sameGround.contains(_)) || other.notVar.exists(sameVar.contains(_)))
      None
    else if (other.sameGround.exists(notGround.contains(_)) || other.sameVar.exists(notVar.contains(_)))
      None
    else if (other.sameGround.isDefined && sameGround.isDefined && other.sameGround.get != sameGround.get)
      None
    else
      Some(new Condition(notGround ++ other.notGround, notVar ++ other.notVar, sameGround.orElse(other.sameGround), sameVar ++ other.sameVar, othersGround ++ other.othersGround, othersVar ++ other.othersVar))

  def uvars: Set[CVar[Type]] =
    notVar.map(_.x) ++ sameVar.map(_.x) ++ othersGround.flatMap(_._2.uvars) ++ othersVar.flatMap(kv => kv._2.uvars + kv._1.x)

  override def toString: String = {
    val not = notGround ++ notVar
    val same = sameGround ++ sameVar
    val others = othersGround ++ othersVar
    val sothers = if (others.isEmpty) "" else s",others(${others.mkString(", ")})"
    s"diff(${not.mkString(", ")}),same(${same.mkString(", ")})$sothers"
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Condition =>
      notGround == other.notGround && notVar == other.notVar && sameGround == other.sameGround && sameVar == other.sameVar && othersGround == other.othersGround && othersVar == other.othersVar
    case _ => false
  }

  override def hashCode(): Int = notGround.hashCode() + 31*notVar.hashCode() + 61*sameGround.hashCode() + 97*sameVar.hashCode() + 127*othersGround.hashCode() + 163*othersVar.hashCode()
}

class Conditional(val cons: Constraint, val cls: Type, val cond: Condition) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = {
    val cls_ = cls.subst(cs.substitution)
    cond.subst(cls_, cs.substitution) match {
      case None => cs // discard this constraint because condition is false
      case Some(cond_) if trueCond == cond_ => cons.solve(cs)
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

  override def hashCode(): Int = cons.hashCode() + 31*cls.hashCode() + 61*cond.hashCode()
}
object Conditional {
  def apply(cons: Constraint, cls: Type, cond: Condition): Constraint =
    if (cond == Condition.trueCond)
      cons
    else
      new Conditional(cons, cls, cond)
}



case class ClassReqs (
                       currentCls: Option[Type] = None,
                       exts: Set[ExtCReq] = Set(),
                       ctors: Set[CtorCReq] = Set(),
                       fields: Map[Symbol, Set[FieldCReq]] = Map(),
                       methods: Map[Symbol, Set[MethodCReq]] = Map(),
                       optMethods: Map[Symbol, Set[MethodCReq]] = Map()) {

  override def toString =
    s"ClassReqs(current=$currentCls, ext=$exts, ctorParams=$ctors, fields=$fields, methods=$methods, optMethods=$optMethods)"

  def subst(s: CSubst): (ClassReqs, Seq[Constraint]) = {
    val currentX = currentCls.map(_.subst(s))
    val extX = exts.flatMap(_.subst(s))
    val ctorX = ctors.flatMap(_.subst(s))
    val (fieldX, cons1) = substMap(s, fields)
    val (methodX, cons2) = substMap(s, methods)
    val (optMethodX, cons3) = substMap(s, optMethods)
    val crs = ClassReqs(currentX, extX, ctorX, fieldX, methodX, optMethodX)
    (crs, cons1 ++ cons2 ++ cons3)
  }

  def substMap[T <: CReq[T] with Named](s: CSubst, m: Map[Symbol, Set[T]]): (Map[Symbol, Set[T]], Seq[Constraint]) = {
    var res = Map[Symbol, Set[T]]()
    var cons = Seq[Constraint]()

    m.foreach { case (sym, set) =>
      val builder = new MapRequirementsBuilder[T]
      for (req <- set;
           sreq <- req.subst(s))
        builder.addReq(sreq)
      res += sym -> builder.getRequirements
      cons ++= builder.getConstraints
    }

    (res, cons)
  }

  def isEmpty =
    currentCls.isEmpty &&
    exts.isEmpty &&
    ctors.isEmpty &&
    fields.values.forall(_.isEmpty) &&
    methods.values.forall(_.isEmpty)

  def merge(crs: ClassReqs): (ClassReqs, Seq[Constraint]) = {
    val (currentX, cons0) = (currentCls.orElse(crs.currentCls), for (t1 <- currentCls; t2 <- crs.currentCls) yield Equal(t1, t2))
    val (extX, cons1) = merge(exts, crs.exts)
    val (ctorX, cons2) = merge(ctors, crs.ctors)
    val (fieldsX, cons3) = mergeMap(fields, crs.fields)
    val (methodsX, cons4) = mergeMap(methods, crs.methods)
    val (optMethodsX, cons5) = mergeMap(optMethods, crs.optMethods)
    val cons = cons0.toSeq ++ cons1 ++ cons2 ++ cons3 ++ cons4 ++ cons5
    (ClassReqs(currentX, extX, ctorX, fieldsX, methodsX, optMethodsX), cons)
  }

  private def merge[T <: CReq[T]](crs1: Set[T], crs2: Set[T]): (Set[T], Seq[Constraint]) = {
    if (crs1.isEmpty)
      return (crs2, Seq())

    Util.loop[Set[T], T, Constraint](addRequirement)(crs1, crs2)
  }

  private def mergeMap[T <: CReq[T] with Named](crs1: Map[Symbol, Set[T]], crs2: Map[Symbol, Set[T]]): (Map[Symbol, Set[T]], Seq[Constraint]) = {
    if (crs1.isEmpty)
      return (crs2, Seq())

    Util.loop[Map[Symbol, Set[T]], T, Constraint](addRequirementMap)(crs1, crs2.values.toStream.flatten)
  }

  def addRequirement[T <: CReq[T]](crs: Set[T], req: T): (Set[T], Seq[Constraint]) = {
    // the new requirement, refined to be different from all existing requirements
    var diffreq: Option[T] = if (crs.size != 1) Some(req) else None
    // new constraints
    var cons = Seq[Constraint]()
    // updated requirements
    val diffcres = crs.flatMap{ creq =>
      if (creq.canMerge(req)) {
        diffreq = diffreq.flatMap(_.alsoNot(creq.cls))

        val diff1 = creq.alsoNot(req.cls)
        val same1 = creq.alsoSame(req.cls)
        val req1 = if (diff1.isEmpty) same1 else if (same1.isEmpty) diff1 else Some(creq)

        val diff2 = req.alsoNot(creq.cls)
        val same2 = req.alsoSame(creq.cls)
        val req2 = if (diff2.isEmpty) same2 else if (same2.isEmpty) diff2 else Some(req)

        val res = Seq() ++ req1 ++ req2

//        // if creq.cls == req.cls then creq.assert(req)
//        val c = None // same1.map(cr => creq.assert(req, cr.cond))
//        cons = cons ++ c

        res
      }
      else
        Seq(creq)
    }
    (diffcres ++ diffreq, cons)
  }

  def addRequirementMap[T <: CReq[T] with Named](crs: Map[Symbol, Set[T]], req: T): (Map[Symbol, Set[T]], Seq[Constraint]) = {
    val set = crs.getOrElse(req.name,
      return (crs + (req.name -> Set(req)), Seq()))

    // the new requirement, refined to be different from all existing requirements
    var diffreq: Option[T] = if (set.size != 1) Some(req) else None

    val builder = new MapRequirementsBuilder[T]

    set.foreach{ creq =>
      diffreq = diffreq.flatMap(_.alsoNot(creq.cls))

      val diff1 = creq.alsoNot(req.cls)
      val same1 = creq.alsoSame(req.cls)
      val req1 = if (diff1.isEmpty) same1 else if (same1.isEmpty) diff1 else Some(creq)

      val diff2 = req.alsoNot(creq.cls)
      val same2 = req.alsoSame(creq.cls)
      val req2 = if (diff2.isEmpty) same2 else if (same2.isEmpty) diff2 else Some(req)

      req1.foreach(builder.addReq)
      req2.foreach(builder.addReq)
    }

    diffreq.foreach(builder.addReq)

    (crs + (req.name -> builder.getRequirements), builder.getConstraints)
  }


  def satisfyCReq[T <: CReq[T]](sat: T, crs: Set[T]): (Set[T], Seq[Constraint]) = {
    var cons = Seq[Constraint]()
    val newcrs = crs flatMap ( creq =>
      if (sat.canMerge(creq)) {
        cons ++= creq.assert(sat)
        creq.alsoNot(sat.cls)
      }
      else
        Some(creq)
      )
    (newcrs, cons)
  }

  def satisfyCReqMap[T <: CReq[T] with Named](sat: T, crs: Map[Symbol, Set[T]]): (Map[Symbol, Set[T]], Seq[Constraint]) = {
    val set = crs.getOrElse(sat.name,
      return (crs, Seq()))

    var cons = Seq[Constraint]()
    val newcrs = set.flatMap( creq =>
      if (sat.canMerge(creq)) {
        cons ++= creq.assert(sat)
        creq.alsoNot(sat.cls)
      }
      else
        Some(creq)
    )
    if (newcrs.isEmpty)
      (crs - sat.name, cons)
    else
      (crs + (sat.name -> newcrs), cons)
  }
}

class MapRequirementsBuilder[T <: CReq[T] with Named] {
  private var newreqs = Map[(Type, Condition), T]()
  private var cons = Set[Constraint]()
  def addReq(req: T) = {
    val key = (req.cls, req.cond)
    newreqs.get(key) match {
      case None => newreqs += (key -> req)
      case Some(oreq) =>
        if (req != oreq) {
          val c = req.assert(oreq, req.cond)
          cons += c
        }
    }
  }

  def getRequirements: Set[T] = newreqs.values.toSet
  def getConstraints: Seq[Constraint] = cons.toSeq
}