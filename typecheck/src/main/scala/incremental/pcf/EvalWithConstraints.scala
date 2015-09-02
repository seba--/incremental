package incremental.pcf

import java.util.Objects

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
  case class VFun(vx: UVar, body: Val, c: Cons) extends Val { // dynamically scoped
    override def toString = s"($vx => $body | ${c.toSeq.mkString(", ")})"
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

    override def hashCode() = 31 * x.hashCode + Objects.hashCode(box.v)

    override def equals(obj: scala.Any) = obj.isInstanceOf[UVar] && {
      val o = obj.asInstanceOf[UVar]
      x == o.x && Objects.equals(box.v, o.box.v) || box == o.box
    }
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
    override def toString = s"$vres = $v1 * $v2"
  }
  case class VApp(vres: UVar)(v1: Val, v2: Val) extends VConstraint {
    override def solve = v1.get match {
      case null => notyet
      case VFun(vx, body, c) =>
        vx.set(v2)
        vres.set(body)
        val rest = c.fixsolve
        if (rest == CEmpty)
          solved{}
        else
          notyet
      case _ => never
    }
    override def toString = s"$vres = $v1($v2)"
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
    override def toString = s"$vres = if0($v1, $v2, $v3)"
  }

  trait Cons {
    val size: Int
    def isEmpty = size == 0
    def ++(cs: Cons): Cons
    def :+(c: VConstraint): Cons = CInsert(c, this)
    def toSeq: Seq[VConstraint]
    def solve: Cons
    def fixsolve: Cons = {
      var rest = this
      var oldsize = rest.size
      var newsize = oldsize
      do {
        oldsize = newsize
        rest = rest.solve
        newsize = rest.size
      } while (newsize < oldsize)
      rest
    }
  }
  case object CEmpty extends Cons {
    val size = 0
    def ++(cs: Cons) = cs
    override def toSeq = Seq()
    def solve = CEmpty
  }
  case class CInsert(c: VConstraint, cs: Cons) extends Cons {
    val size = cs.size + 1
    override def ++(cs: Cons) = CConcat(Seq(this, cs))
    override def toSeq = c +: cs.toSeq
    override def solve =
      if (c.solve)
        cs.solve
      else
        cs.solve :+ c
  }
  case class CConcat(css: Seq[Cons], _size: Int = -1) extends Cons {
    val size =
      if (_size >= 0)
        _size
      else
        css.foldLeft[Int](0)((n,cs) => n + cs.size)
    override def ++(cs: Cons) = CConcat(css :+ cs, size + cs.size)
    override def toSeq = css.foldLeft[Seq[VConstraint]](Seq())((seq,cs) => seq ++ cs.toSeq)
    override def solve = css.foldLeft[Cons](CEmpty)((res,cs) => res ++ cs.solve)
  }

  type Reqs = Map[Symbol, UVar]
  type StepResult = (Val, Reqs, Cons)


  def eval(e: Node): Either[Val, String] = {
    val root = e.withType[StepResult]
    root.visitUninitialized { e =>
      e.typ = evalStep(e)
      true
    }

    val (v, r, c) = root.typ

    val rest = c.fixsolve
    if (rest.isEmpty && r.isEmpty)
      Left(v.get)
    else
      Right(s"Unsolved requirements=$r, value=$v, unsolved constraints=${rest.toSeq}")
  }

  def evalStep(e: Node_[StepResult]): StepResult = e.kind match {
    case Num =>
      (VNum(e.lits(0).asInstanceOf[Integer]), Map(), CEmpty)

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
      (v, Map(x -> v), CEmpty)

    case Abs =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val (v, r, c) = e.kids(0).typ
      val crest = c.solve
      r.get(x) match {
        case None => (VFun(freshUVar(), v, crest), r, CEmpty)
        case Some(vx) => (VFun(vx, v, crest), r - x, CEmpty)
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
      (v, mr, c1 ++ c2 ++ c3 ++ mc :+ VIf0(v)(v1, v2, v3))
  }










  private val init: (Cons, Reqs) = (CEmpty, Map())

  def mergeReqMaps(req: Reqs, reqs: Reqs*): (Cons, Reqs) = mergeReqMaps(req +: reqs)

  def mergeReqMaps(reqs: Seq[Reqs]): (Cons, Reqs) =
    reqs.foldLeft[(Cons, Reqs)](init)(_mergeReqMaps)

  private def _mergeReqMaps(was: (Cons, Reqs), newReqs: Reqs) = {
    val wasReqs = was._2
    var mcons = was._1
    var mreqs = wasReqs
    for ((x, r2) <- newReqs)
      wasReqs.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) =>
          mcons = mcons :+ VEq(r1)(r2)
      }
    (mcons, mreqs)
  }
}
