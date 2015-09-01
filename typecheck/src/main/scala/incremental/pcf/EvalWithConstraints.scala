package incremental.pcf

import constraints.equality.Gen
import incremental.Node.Node
import incremental.Node_

object EvalWithConstraints {

  class Box[T](var v: T) {
    override def toString = s"Box($v)"
  }
  
  trait Val {
    def get = this
  }
  case class VNum(n: Integer) extends Val {
    override def toString = n.toString
  }
  case class VFun(vx: UVar, body: Val) extends Val { // dynamically scoped
    override def toString = s"($vx => $body)"
  }
  case class UVar(x: Symbol, private var box: Box[Val] = new Box(null)) extends Val {
    override def get = box.v
    def set(v: Val): Unit = {
      if (box.v != null) // just for debugging
        throw new IllegalStateException(s"Attempt to overwrite UVar $this")
      if (v.isInstanceOf[UVar])
        box = v.asInstanceOf[UVar].box
      else
        box.v = v
    }
    def isSet = box.v != null
    override def toString =
      if (isSet)
        get.toString
      else
        "$" + x.name
  }

  val gen = new Gen
  def freshUVar() = UVar(gen.freshSymbol[Val]("v").x)

  trait VConstraint {
    def solve: Boolean
    def solved(f: =>Unit) = {f; true}
    def notyet = false
    def never = sys.error(s"Cannot solve $this")
  }

  case class VEq(v1: UVar)(v2: UVar) extends VConstraint {
    override def solve = {
      if (!v1.isSet)
        solved(v1.set(v2))
      else if (!v2.isSet)
        solved(v2.set(v1))
      else if (v1.get == v2.get)
        solved{}
      else
        never
    }
  }
  case class VAdd(vres: UVar)(v1: Val, v2: Val) extends VConstraint {
    override def solve = (v1.get, v2.get) match {
      case (null,_) | (_,null) => notyet
      case (VNum(n1), VNum(n2)) => solved(vres.set(VNum(n1 + n2)))
      case _ => never
    }
    override def toString = s"$vres = $v1 + $v2"
  }
  case class VMul(vres: UVar)(v1: Val, v2: Val) extends VConstraint {
    override def solve = (v1.get, v2.get) match {
      case (null,_) | (_,null) => notyet
      case (VNum(n1), VNum(n2)) => solved(vres.set(VNum(n1 * n2)))
      case _ => never
    }
  }
  case class VApp(vres: UVar)(v1: Val, v2: Val) extends VConstraint {
    override def solve = v1.get match {
      case null => notyet
      case VFun(vx, body) => solved {vx.set(v2); vres.set(body)}
      case _ => never
    }
  }
  case class VIf0(vres: UVar)(v1: Val, v2: Val, v3: Val) extends VConstraint {
    override def solve = v1.get match {
      case null => notyet
      case VNum(n) =>
        if (n == 0)
          solved(vres.set(v2))
        else
          solved(vres.set(v3))
      case _ => never
    }
  }

  type Cons = Seq[VConstraint]
  type Reqs = Map[Symbol, UVar]
  type StepResult = (Val, Reqs, Cons)


  def eval(e: Node): Either[Val, String] = {
    val root = e.withType[StepResult]
    root.visitUninitialized { e =>
      e.typ = evalStep(e)
      true
    }

    val (v, r, c) = root.typ

    var oldsize = c.size
    var newsize = oldsize
    var unsolved = c
    do {
      oldsize = newsize
      unsolved = unsolved.filter(!_.solve)
      newsize = unsolved.size
    } while (newsize < oldsize)

    if (unsolved.isEmpty && r.isEmpty)
      Left(v.get)
    else
      Right(s"Unsolved requirements=$r, value=$v, unsolved constraints=$unsolved")
  }

  def evalStep(e: Node_[StepResult]): StepResult = e.kind match {
    case Num =>
      (VNum(e.lits(0).asInstanceOf[Integer]), Map(), Seq())

    case Add =>
      val (v1, r1, c1) = e.kids(0).typ
      val (v2, r2, c2) = e.kids(1).typ
      val v = freshUVar()
      val (mc, mr) = mergeReqMaps(r1, r2)
      (v, mr, c1 ++ c2 ++ mc :+ VAdd(v)(v1, v2))

    case Mul =>
      val (v1, r1, c1) = e.kids(0).typ
      val (v2, r2, c2) = e.kids(1).typ
      val v = freshUVar()
      val (mc, mr) = mergeReqMaps(r1, r2)
      (v, mr, c1 ++ c2 ++ mc :+ VMul(v)(v1, v2))

    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val v = freshUVar()
      (v, Map(x -> v), Seq())

    case Abs =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val (v, r, c) = e.kids(0).typ
      r.get(x) match {
        case None => (VFun(freshUVar(), v), r, c)
        case Some(vx) => (VFun(vx, v), r - x, c)
      }

    case App =>
      val (v1, r1, c1) = e.kids(0).typ
      val (v2, r2, c2) = e.kids(1).typ
      val v = freshUVar()
      val (mc, mr) = mergeReqMaps(r1, r2)
      (v, mr, c1 ++ c2 ++ mc :+ VApp(v)(v1, v2))

    case If0 =>
      val (v1, r1, c1) = e.kids(0).typ
      val (v2, r2, c2) = e.kids(1).typ
      val (v3, r3, c3) = e.kids(2).typ
      val v = freshUVar()
      val (mc, mr) = mergeReqMaps(r1, r2, r3)
      (v, mr, c1 ++ c2 ++ c3++ mc :+ VIf0(v)(v1, v2, v3))
  }










  private val init: (Seq[VConstraint], Reqs) = (Seq(), Map())

  def mergeReqMaps(req: Reqs, reqs: Reqs*): (Seq[VConstraint], Reqs) = mergeReqMaps(req +: reqs)

  def mergeReqMaps(reqs: Seq[Reqs]): (Seq[VConstraint], Reqs) =
    reqs.foldLeft[(Seq[VConstraint], Reqs)](init)(_mergeReqMaps)

  private def _mergeReqMaps(was: (Seq[VConstraint], Reqs), newReqs: Reqs) = {
    val wasReqs = was._2
    var mcons = was._1
    var mreqs = wasReqs
    for ((x, r2) <- newReqs)
      wasReqs.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) =>
          mcons = VEq(r1)(r2) +: mcons
      }
    (mcons, mreqs)
  }
}
