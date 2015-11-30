package incremental

import Node._
import constraints.{Constraint, ConstraintSystem}

abstract class NodeKind[C, T](val syntaxcheck: SyntaxChecking.SyntaxCheck) extends Serializable {
  def unapplySeq(e: Node_[C, _, T]): Option[Seq[Node_[C, _, T]]] =
    if (e.kind == this)
      Some(e.kids.seq)
    else
      None

  //def check(lits: Seq[Any], kids: Seq[Node_[_, T]]): T
  def check(lits: Seq[Any], kids: Seq[Node_[C, _, T]], context: Context[C]): T
}

class Node_[C, CS, T](val kind: NodeKind[C, T], val lits: Seq[Lit], kidsArg: Seq[Node_[C, CS, T]]) extends Serializable {
  kind.syntaxcheck(kind).check(lits, kidsArg)

  protected def maxHeight(seq: Seq[Node_[C, CS, T]]): Int = {
    val incr = if (seq.isEmpty) 0 else 1
    seq.foldLeft(0){ case (i, n) => i.max(n._height) } + incr
  }

  protected def sumSize(seq: Seq[Node_[C, CS, T]]): Int = {
    seq.foldLeft(1){ case (s, n) => s + n.size }
  }

  private var _height: Int = maxHeight(kidsArg)
  private var _size = sumSize(kidsArg)
  private var _typ: T = _
  private var _cs: CS = _
  private var _valid = false

  def height = _height

  def size = _size
  def valid = _valid // needed for propagation pruning
  def typ = _typ
  def cs = _cs
  def set(cs: CS, t: T): Unit = {
    _typ = t
    _cs = cs
    _valid = true
  }
  def invalidate: Unit = {
    _typ = null.asInstanceOf[T]
    _valid = false
    _kids foreach (_.invalidate)
  }

  private val _kids: Array[Node_[C, CS, T]] = Array(kidsArg:_*)
  private var availableKidTypes: Seq[Boolean] = kidsArg map (_.typ != null)

  object kids {
    def apply(i: Int) = _kids(i)
    def update[C1, CS1, U](i: Int, e: Node_[C1, CS1, U]): Unit = {
      val ee = e.asInstanceOf[Node_[C, CS, T]]
      if (ee._valid)
        _valid = false
      else
        ee._typ = kids(i)._typ
      _kids(i) = ee
      _height = maxHeight(_kids)
      _size = sumSize(_kids)
    }
    def seq: Seq[Node_[C, CS, T]] = _kids
  }

  def typs(i: Int) = _kids(i)._typ

  def withType[T] = this.asInstanceOf[Node_[_, _, T]]
  def withTypes[C, CS, T] = this.asInstanceOf[Node_[C, CS, T]]

  def markKidTypeAvailable(pos: Int) =
    availableKidTypes = availableKidTypes.updated(pos, true)

  def allKidTypesAvailable = availableKidTypes.foldLeft(true)(_&&_)

  def uninitialized: Seq[Node_[C, CS, T]] = {
    val buf = collection.mutable.ArrayBuffer[Node_[C, CS, T]]()
    uninitialized(buf)
    buf
  }

  def uninitialized(buf: collection.mutable.ArrayBuffer[Node_[C, CS, T]]): Unit = {
    val oldsize = buf.size
    _kids foreach (_.uninitialized(buf))
    val hasSubchange = oldsize == buf.size
    if (!valid || hasSubchange)
      buf += this
  }

  def visitUninitialized(f: Node_[C, CS, T] => Boolean): Boolean = {
    val hasSubchange = _kids.foldLeft(false)((changed, k) =>  k.visitUninitialized(f) || changed)
    if (!valid || hasSubchange)
      f(this)
    else
      false
  }

  def visitUninitialized2(f: Node_[C, CS, T] => (T, Boolean)): Boolean = {
    val hasSubchange = _kids.foldLeft(false)((changed, k) =>  k.visitUninitialized2(f) || changed)
    if (!valid || hasSubchange) {
      val (t, doContinue) = f(this)
      _typ = t
      _valid = true
      doContinue
    }
    else
      false
  }

  def visitInvalid(f: Node_[C, CS, T] => Boolean): Boolean = {
    var hasSubchange = false
    for(k <- _kids if !k.valid) {
      hasSubchange = k.visitInvalid(f)  || hasSubchange
    }
    if (!valid || hasSubchange)
      f(this)
    else
      false
  }

  def visitUpto(depth: Int)(f: Node_[C, CS, T] => Boolean): Boolean = {
    if (depth > 0) {
      val hasSubchange = _kids.foldLeft(false){ (changed, k) => k.visitUpto(depth - 1)(f) || changed }
      if (!valid || hasSubchange)
        f(this)
      else false
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

object Node {
  type Lit = Any
  type Node[C, T] = Node_[C, Any, T]

  import scala.language.implicitConversions
  implicit def kindExpression[C, T](k: NodeKind[C, T]) = new KindExpression(k)
  class KindExpression[C, T](k: NodeKind[C, T]) {
    def apply() = new Node_[C, Any, T](k, Seq(), Seq())
    def apply(l: Lit, sub: Node[C, T]*) = new Node_[C, Any, T](k, scala.Seq(l), Seq(sub:_*))
    def apply(l1: Lit, l2: Lit, sub: Node[C, T]*) = new Node_[C, Any, T](k, scala.Seq(l1, l2), Seq(sub:_*))
    def apply(e: Node[C, T], sub: Node[C, T]*) = new Node_[C, Any, T](k, scala.Seq(), e +: Seq(sub:_*))
    def apply(lits: Seq[Lit], sub: Seq[Node[C, T]]) = new Node_[C, Any, T](k, lits, sub)
  }
  
  val ignore = (k: NodeKind[_, _]) => new SyntaxChecking.IgnoreSyntax(k)
  def simple(kidTypes: Class[_ <: NodeKind[_, _]]*) = (k: NodeKind[_, _]) => new SyntaxChecking.KidTypesLitTypesSyntax(k, Seq(), Seq(kidTypes:_*))
  def simple(litTypes: Seq[Class[_]], kidTypes: Class[_ <: NodeKind[_, _]]*) = (k: NodeKind[_, _]) => new SyntaxChecking.KidTypesLitTypesSyntax(k, litTypes, Seq(kidTypes:_*))
  implicit def makeSyntaxCheckOps(f: SyntaxChecking.SyntaxCheck) = new SyntaxChecking.SyntaxCheckOps(f)
}

object SyntaxChecking {
  type SyntaxCheck = NodeKind[_, _] => SyntaxChecker

  abstract class SyntaxChecker(k: NodeKind[_, _]) {
    class SyntaxError(val k: NodeKind[_, _], val msg: String) extends IllegalArgumentException(msg) {
      override def getMessage(): String = s"Syntax error in $k node: $msg"
    }
    def error(msg: String) = throw new SyntaxError(k, msg)
    def check[C, CS, T](lits: Seq[Lit], kids: Seq[Node_[C, CS, T]])
  }

  class SyntaxCheckOps(f: NodeKind[_, _] => SyntaxChecker) {
    def orElse(g: NodeKind[_, _] => SyntaxChecker) = (k: NodeKind[_, _]) => new AlternativeSyntax(k, f, g)
    def andAlso(g: NodeKind[_, _] => SyntaxChecker) = (k: NodeKind[_, _]) => new ConjunctiveSyntax(k, f, g)
  }

  class IgnoreSyntax(k: NodeKind[_, _]) extends SyntaxChecker(k) {
    def check[C, CS, T](lits: Seq[Lit], kids: Seq[Node_[C, CS, T]]) = {}
  }

  case class KidTypesLitTypesSyntax(k: NodeKind[_, _], litTypes: Seq[Class[_]], kidTypes: Seq[Class[_ <: NodeKind[_, _]]]) extends SyntaxChecker(k) {
    def check[C, CS, T](lits: Seq[Lit], kids: Seq[Node_[C, CS, T]]) {
      if (kids.size != kidTypes.size)
        error(s"Expected ${kidTypes.size} subexpressions but found ${kids.size} subexpressions")

      for (i <- 0 until kids.size)
        if (!kidTypes(i).isInstance(kids(i).kind))
          error(s"Expected kid of ${kidTypes(i)} at position $i but found ${kids(i)} of ${kids(i).kind.getClass}")

      if (lits.size != litTypes.size)
        error(s"Expected ${litTypes.size} literals but found ${lits.size} literals")

      for (i <- 0 until lits.size)
        if (!litTypes(i).isInstance(lits(i)))
          error(s"Expected literal of ${litTypes(i)} at position $i but found ${lits(i)} of ${lits(i).getClass}")
    }
  }

  case class AlternativeSyntax(k: NodeKind[_, _], f: NodeKind[_, _] => SyntaxChecker, g: NodeKind[_, _] => SyntaxChecker) extends SyntaxChecker(k) {
    def check[C, CS, T](lits: Seq[Lit], kids: Seq[Node_[C, CS, T]]): Unit = {
      try {
        f(k).check(lits, kids)
      } catch {
        case e1: SyntaxError => try {
          g(k).check(lits, kids)
        } catch {
          case e2: SyntaxError => error(s"Alternative syntax failed \n\t${e1.msg}\nor\n\t${e2.msg})")
        }
      }
    }
  }

  case class ConjunctiveSyntax(k: NodeKind[_, _], f: NodeKind[_, _] => SyntaxChecker, g: NodeKind[_, _] => SyntaxChecker) extends SyntaxChecker(k) {
    def check[C, CS, T](lits: Seq[Lit], kids: Seq[Node_[C, CS, T]]): Unit = {
      try {
        f(k).check(lits, kids)
      } catch {
        case e1: SyntaxError => {
          try {
            g(k).check(lits, kids)
          } catch {
            case e2: SyntaxError => error(s"Conjunctive syntax failed \n\t${e1.msg}\nand\n\t${e2.msg})")
          }
          error(s"Conjunctive syntax failed \n\t${e1.msg}")
        }
      }

      try {
        g(k).check(lits, kids)
      } catch {
        case e: SyntaxError => error(s"Conjunctive syntax failed \n\t${e.msg}")
      }
    }
  }
}