package algebra

object Sig {
  trait Arith[E] {
    def num(n: Int): E
    def add(e1: E, e2: E): E
    def mul(e1: E, e2: E): E
    def if0(c: E, t: E, e: E): E
  }

  trait Fun[E] {
    def va(x: String): E
    def abs(x: String, e: E): E
    def app(e1: E, e2: E): E
  }

  trait Fix[E] {
    def fix(e: E): E
  }
}

object Programs {
  import Sig._

  def fact[E](alg: Arith[E] with Fun[E] with Fix[E]) = {
    import alg._
    fix(abs("f", abs("x",
      if0(va("x"),
        num(1),
        mul(va("x"), app(va("f"), add(va("x"), num(-1))))))))
  }

  def fact5[E](alg: Arith[E] with Fun[E] with Fix[E]) = {
    import alg._
    app(fact(alg), num(5))
  }
}

object Eval {
  type Env = collection.Map[String, Val]

  trait Val
  case class NumVal(n: Int) extends Val
  case class FunVal(x: String, e: Env => Val, env: Env) extends Val {
    override def toString = s"FunVal($x, $e, _env_)"
  }

  type Dom = Env => Val

  trait Arith extends Sig.Arith[Dom] {
    override def num(n: Int) = _ => NumVal(n)

    override def add(e1: Dom, e2: Dom) = env => (e1(env), e2(env)) match {
      case (NumVal(n1), NumVal(n2)) => NumVal(n1 + n2)
    }

    override def mul(e1: Dom, e2: Dom) = env => (e1(env), e2(env)) match {
      case (NumVal(n1), NumVal(n2)) => NumVal(n1 * n2)
    }

    override def if0(c: Dom, t: Dom, e: Dom) = env => c(env) match {
      case NumVal(n) =>
        if (n == 0)
          t(env)
        else
          e(env)
    }
  }

  trait Fun extends Sig.Fun[Dom] {
    def lookup(env: Env, x: String) = env(x)
    override def va(x: String) = env => lookup(env, x)

    override def abs(x: String, e: Dom) = env => FunVal(x, e, env)

    override def app(e1: Dom, e2: Dom) = env => (e1(env), e2(env)) match {
      case (FunVal(x, e, eenv), v) => e(eenv + (x -> v))
    }
  }

  trait Fix extends Sig.Fix[Dom] {
    override def fix(e: Dom) = env => e(env) match {
      case v@FunVal(x, fe, fenv) =>
        val menv = collection.mutable.Map() ++= fenv
        val r = fe(menv)
        menv += (x -> r)
        r
    }
  }

  object ArithFunFix extends Arith with Fun with Fix
}


object Types {
  trait Type
  case object TNum extends Type
  case class TFun(from: Type, to: Type) extends Type
  case class UVar(x: String) extends Type

  case class EqCon(t1: Type, t2: Type)
  type Cons = Seq[EqCon]
  type Reqs = Map[String, Type]

  private val init: (Seq[EqCon], Reqs) = (Seq(), Map())
  def merge(req: Reqs, reqs: Reqs*): (Seq[EqCon], Reqs) = mergeReqMaps(req +: reqs)
  def mergeReqMaps(reqs: Seq[Reqs]): (Seq[EqCon], Reqs) = reqs.foldLeft[(Seq[EqCon], Reqs)](init)(_mergeReqMaps)
  private def _mergeReqMaps(was: (Seq[EqCon], Reqs), newReqs: Reqs) = {
    val wasReqs = was._2
    var mcons = was._1
    var mreqs = wasReqs
    for ((x, r2) <- newReqs)
      wasReqs.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) =>
          mcons = EqCon(r1, r2) +: mcons
      }
    (mcons, mreqs)
  }

  case class CoType(t: Type, cons: Cons, reqs: Reqs) {
//    def solve: Either[Type, String] = {
//      if (reqs.nonEmpty)
//        Right(s"Unresolved requirements $reqs")
//
//
//    }
  }

  trait Arith extends Sig.Arith[CoType] {
    override def num(n: Int) = CoType(TNum, Seq(), Map())

    override def add(e1: CoType, e2: CoType) = {
      val cons = e1.cons ++ e2.cons :+ EqCon(e1.t, TNum) :+ EqCon(e2.t, TNum)
      val (mcons, mreqs) = merge(e1.reqs, e2.reqs)
      CoType(TNum, cons ++ mcons, mreqs)
    }

    override def mul(e1: CoType, e2: CoType) = add(e1, e2)

    override def if0(c: CoType, t: CoType, e: CoType) = {
      val cons = c.cons ++ t.cons ++ e.cons :+ EqCon(c.t, TNum) :+ EqCon(t.t, e.t)
      val (mcons, mreqs) = merge(c.reqs, t.reqs, e.reqs)
      CoType(t.t, cons ++ mcons, mreqs)
    }
  }

//  trait Arith2[C, R, CT <: ICoType[C, R]] extends Sig.Arith[CT] {
//    val alg: CoTypeAlg[C, R, CT]
//    import alg._
//
//    override def num(n: Int) = cotype(TNum, emptyC, emptyR)
//
//    override def add(e1: CT, e2: CT) = {
//      val cons = concat(e1.cons, concat(e2.cons, concat(teq(e1.t, TNum), teq(e2.t, TNum))))
//      val (mcons, mreqs) = merge(e1.reqs, e2.reqs)
//      cotype(TNum, concat(cons, mcons), mreqs)
//    }
//
//    override def mul(e1: CT, e2: CT) = add(e1, e2)
//
////    override def if0(c: CT, t: CT, e: CT) = {
////      val cons = c.cons ++ t.cons ++ e.cons :+ EqCon(c.t, TNum) :+ EqCon(t.t, e.t)
////      val (mcons, mreqs) = merge(c.reqs, t.reqs, e.reqs)
////      CoType(t.t, cons ++ mcons, mreqs)
////    }
//  }

  trait ICoType[C, R] {
    def t: Type
    def cons: C
    def reqs: R
  }
  trait CoTypeAlg[C, R, CT <: ICoType[C, R]] {
    type T = Type
    def cotype(t: T, c: C, r: R): CT
    def merge(r1: R, r2: R): (C, R)
    def concat(c1: C, c2: C): C

    def emptyC: C
    def teq(t1: T, t2: T): C
    def emptyR: R
    def req(x: String, t: T): R
  }

  var id = 0
  def gensym(x: String) = x + (id += 1)

  trait Fun extends Sig.Fun[CoType] {
    override def va(x: String) = {
      val t = UVar(gensym("x"));
      CoType(t, Seq(), Map(x -> t))
    }

    override def abs(x: String, e: CoType) = {
      val argType = UVar(gensym("x"));
      val t = TFun(argType, e.t)
      CoType(t, e.cons, e.reqs - x)
    }

    override def app(e1: CoType, e2: CoType) = {
      val x = UVar(gensym("x"))
      val cons = e1.cons ++ e2.cons :+ EqCon(e1.t, TFun(e2.t, x))
      val (mcons, mreqs) = merge(e1.reqs, e2.reqs)
      CoType(x, cons ++ mcons, mreqs)
    }
  }

  trait Fix extends Sig.Fix[CoType] {
    override def fix(e: CoType) = {
      val x = UVar(gensym("x"))
      CoType(x, e.cons :+ EqCon(e.t, TFun(x, x)), e.reqs)
    }
  }

  object ArithFunFix extends Arith with Fun with Fix
}



object Test extends App {
  override def main(args: Array[String]) = {
    println("fact: " + Programs.fact(Eval.ArithFunFix)(Map()))
    println("fact5: " + Programs.fact5(Eval.ArithFunFix)(Map()))

    println("unbound x: " + Types.ArithFunFix.va("x"))
    println("fact: " + Programs.fact(Types.ArithFunFix))
    println("fact5: " + Programs.fact5(Types.ArithFunFix))
  }
}