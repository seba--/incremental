package incremental.pcf.let_poly

import constraints.CVar
import constraints.equality_letpoly._
import constraints.equality_letpoly.impl._
import incremental.Node._
import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
 * Created by seba on 14/11/14.
 */
class TestCorrectness[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: TypeCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
  val checker: TypeChecker[CS] = checkerFactory.makeChecker

  override def afterEach: Unit = checker.localState.printStatistics()

  def typecheckTest(desc: String, e: =>Node)(expected: Type): Unit =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isLeft, s"Expected $expected but got $actual")

      val sol = SolveContinuously.state.withValue(checker.csFactory.state.value) {
        expected.unify(actual.left.get, SolveContinuously.freshConstraintSystem).tryFinalize
      }
      assert(sol.isSolved, s"Expected $expected but got ${actual.left.get}. Match failed with ${sol.unsolved}")
    }

  def typecheckTestError(desc: String, e: =>Node) =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }

  typecheckTest("17", Num(17))(TNum)
  typecheckTest("17+(10+2)", Add(Num(17), Add(Num(10), Num(2))))(TNum)
  typecheckTest("17+(10+5)", Add(Num(17), Add(Num(10), Num(5))))(TNum)
  typecheckTest("\\x. 10+5", Abs('x, Add(Num(10), Num(5))))(TFun(UVar(CVar('x$0)), TNum))
  typecheckTest("\\x. x+x", Abs('x, Add(Var('x), Var('x))))(TFun(TNum, TNum))
  typecheckTestError("\\x. err+x", Abs('x, Add(Var('err), Var('x))))
  typecheckTest("\\x. \\y. x y", Abs('x, Abs('y, App(Var('x), Var('y)))))(TFun(TFun(UVar(CVar('x$1)), UVar(CVar('x$2))), TFun(UVar(CVar('x$1)), UVar(CVar('x$2)))))
  typecheckTest("\\x. \\y. x + y", Abs('x, Abs('y, Add(Var('x), Var('y)))))(TFun(TNum, TFun(TNum, TNum)))
  typecheckTest("if0(17, 0, 1)", If0(Num(17), Num(0), Num(1)))(TNum)
  typecheckTestError("\\x. x + (x 5)", Abs('x, Add(Var('x), App(Var('x), Num(5)))))

  lazy val mul = Fix(Abs('f, TFun(TNum, TFun(TNum, TNum)),
                   Abs('m, TNum, Abs('n, TNum,
                     If0(Var('m), Num(0), App(App(Var('f), Add(Var('m), Num(-1))), Var('n)))))))
  typecheckTest("multiplication", mul)(TFun(TNum, TFun(TNum, TNum)))

  lazy val fac = Fix(Abs('f, Abs('n, If0(Add(Var('n), Num(-1)), Num(1), App(App(mul, Var('n)), App(Var('f), Add(Var('n), Num(-2))))))))
  typecheckTest("factorial", fac)(TFun(TNum, TNum))

  lazy val fac1 = Fix(Abs('f, Abs('n, If0(Add(Var('n), Num(-1)), Num(1), App(App(mul, Var('n)), App(Var('f), Add(Var('n), Num(-2))))))))
  typecheckTest("factorial1", fac1)(TFun(TNum, TNum))
  lazy val fac2 = {fac1.kids(0).kids(0).kids(0).kids(0) = Var('n); fac1}
  typecheckTest("factorial2", fac2)(TFun(TNum, TNum))
  lazy val fac3 = {fac1.kids(0).kids(0).kids(0).kids(2).kids(1).kids(1).kids(1) = Num(-1); fac1}
  typecheckTest("factorial3", fac3)(TFun(TNum, TNum))
  lazy val fac3_2 = {
    val absn = fac1.kids(0).kids(0)
    fac1.kids(0).kids(0) = Abs('x, absn.kids(0))
    fac1
  }
  //typecheckTestError("factorial3_2", fac3_2)
  lazy val fac4 = Fix(Abs('f, Abs('n, If0(Add(Var('n), Num(-1)), Num(1), App(App(mul, Var('n)), App(Var('f), Add(Var('n), Num(-2))))))))
  typecheckTest("factorial4", fac4)(TFun(TNum, TNum))
  lazy val fac5 = {fac4.kids(0).kids(0).kids(0).kids(0) = Var('n); fac4.kids(0).kids(0).kids(0).kids(2).kids(1).kids(1).kids(1) = Num(-1); fac4}
  typecheckTest("factorial5", fac5)(TFun(TNum, TNum))

  typecheckTest("eta-expanded factorial", Abs('x, App(fac, Var('x))))((TFun(TNum, TNum)))

  lazy val fib = Fix(Abs('f, Abs('n,
    If0(Var('n), Num(1),
      If0(Add(Var('n), Num(-1)), Num(1),
        Add(App(Var('f), Add(Var('n), Num(-1))),
          App(Var('f), Add(Var('n), Num(-2)))))))))
  typecheckTest("fibonacci", fib)(TFun(TNum, TNum))
  typecheckTest("factorial + fibonacci", Abs('x, Add(App(fac, Var('x)), App(fib, Var('x)))))(TFun(TNum, TNum))
  typecheckTest("\\y. y", Abs('y, Var('y)))(TFun(UVar(CVar('x$0)), UVar(CVar('x$0))))
  typecheckTestError("\\x. x x", Abs('x, App(Var('x), Var('x))))

  typecheckTest("let x = 2 in x + 1", LetV('x, Num(2), Add(VarL('x), Num(1))))(TNum)
  typecheckTest("let y = 1 in let x = y in x + 1", LetV('y, Num(1), LetV('x, VarL('y), Add(VarL('x), Num(1)))))(TNum)
  typecheckTestError("let y = 'a in let x = y in x + 1", LetV('y, Char('a), LetV('x, VarL('y), Add(VarL('x), Num(1)))))
  typecheckTest("let chLen = \\Char. Int in let y = 'a in let x = y in chLen x", LetV('chLen, Abs('a, TChar, Num(1)),
    LetV('y, Char('a), LetV('x, VarL('y), App(VarL('chLen), VarL('x))))))(TNum)
  typecheckTestError("let chLen = \\Char. Int in let y = 1 in let x = y in chLen x", LetV('chLen, Abs('a, TChar, Num(1)),
    LetV('y, Num(1), LetV('x, VarL('y), App(VarL('chLen), VarL('x))))))
  typecheckTestError("let chLen = \\Char. Int in let y = 'a in let x = y in chLen x + x", LetV('chLen, Abs('a, TChar, Num(1)),
    LetV('y, Char('a), LetV('x, VarL('y), Add(App(VarL('chLen), VarL('x)), VarL('x))))))
  typecheckTestError("let chLen = \\Char. Int in let y = 1 in let x = y in chLen x + x", LetV('chLen, Abs('a, TChar, Num(1)),
    LetV('y, Num(1), LetV('x, VarL('y), Add(App(VarL('chLen), VarL('x)), VarL('x))))))
  typecheckTestError("let y = 'a' in let x = y in x + 1", LetV('y, Char('a), LetV('x, VarL('y), Add(VarL('x), Num(1)))))
  typecheckTest("let x = \\y .y in x x", LetV('x, Abs('y, VarL('y)), App(VarL('x), VarL('x))))(TFun(TNum, TNum))
  typecheckTest("let x = \\a.a in y = x 0 in 1 + y ", LetV('x , Abs('a, VarL('a)), LetV('y, App(VarL('x), Num(0)), Add(VarL('y), Num(1)))))(TNum)
  typecheckTestError("let x = \\a.a in y = x 'b in 1 + y ", LetV('x , Abs('a, VarL('a)), LetV('y, App(VarL('x), Char('b)), Add(VarL('y), Num(1)))))
  typecheckTest("let chLen = \\ Char. Int in let x = \\a.a in let y = x 'b in ChLen y", LetV('chLen, Abs('a, TChar, Num(1)),
   LetV('x , Abs('a, VarL('a)), LetV('y, App(VarL('x), Char('b)), App(VarL('chLen), VarL('y))))))(TNum)
  typecheckTest("let x = \\a.a in App  x 'b", LetV('x , Abs('a, Var('a)), App(VarL('x), Char('b))))(TChar)
  typecheckTest("let x = \\a.a in x ", LetV('x , Abs('a, Var('a)), VarL('x)))(TFun(TNum, TNum))
   typecheckTestError("let chLen = \\ Char. Int in let x = \\a.a in let y = x 1 in ChLen y", LetV('chLen, Abs('a, TChar, Num(1)),
    LetV('x , Abs('a, VarL('a)), LetV('y, App(VarL('x), Num(1)), App(VarL('chLen), VarL('y))))))
  typecheckTest("let chLen = \\ Char. Int in let x = \\a.a in let y = x 'b in ChLen y + 1 ", LetV('chLen, Abs('a, TChar, Num(1)),
    LetV('x , Abs('a, VarL('a)), LetV('y, App(VarL('x), Char('b)), Add(App(VarL('chLen), VarL('y)), Num(1))))))(TNum)
  typecheckTestError("let chLen = \\ Char. Int in let x = \\a.a in let y = x 1 in ChLen y + 1 ", LetV('chLen, Abs('a, TChar, Num(1)),
    LetV('x , Abs('a, VarL('a)), LetV('y, App(VarL('x), Num(1)), Add(App(VarL('chLen), VarL('y)), Num(1))))))
  typecheckTestError("let chLen = \\ Char. Int in let x = \\a.a in let y = x 'b in ChLen y + y", LetV('chLen, Abs('a, TChar, Num(1)),
    LetV('x , Abs('a, VarL('a)), LetV('y, App(VarL('x), Char('b)), Add(App(VarL('chLen), VarL('y)), VarL('y))))))
  typecheckTestError("let chLen = \\ Char. Int in let x = \\a.a in let y = x 'b in chLen (ChLen y)", LetV('chLen, Abs('a, TChar, Num(1)),
    LetV('x , Abs('a, VarL('a)), LetV('y, App(VarL('x), Char('b)), App(VarL('chLen), App(VarL('chLen), VarL('y)))))))
  typecheckTestError("let chLen = \\ Char. Int in let x = \\a.a in let y = x 1 in ChLen y + Chlen y ", LetV('chLen, Abs('a, TChar, Num(1)),
    LetV('x , Abs('a, VarL('a)), LetV('y, App(VarL('x), Num(1)), Add(App(VarL('chLen), VarL('y)), App(VarL('chLen), VarL('y)))))))
  typecheckTest("let chLen = \\ Char. Int in let x = \\a.a in let y = x 'b in ChLen y + ChLen y  ", LetV('chLen, Abs('a, TChar, Num(1)),
    LetV('x , Abs('a, VarL('a)), LetV('y, App(VarL('x), Char('b)), Add(App(VarL('chLen), VarL('y)), App(VarL('chLen), VarL('y)))))))(TNum)
  typecheckTest("let plus = \\ Int. \\ Int. Int in let x = \\a. a in let y = x 1 in let plus y y ", LetV('plus, Abs('f, TFun(TNum, TNum), Num(2)),
    LetV('x, Abs('a, VarL('a)), LetV('y, App(VarL('x), Num(1)), App(VarL('plus), Abs('f, TNum,  VarL('y)))))))(TNum)
  typecheckTestError("let plus = \\ Int. \\ Int. Int in let x = \\a. a in let y = x 'b in let plus y -> y ", LetV('plus, Abs('f, TFun(TNum, TNum), Num(2)),
    LetV('x, Abs('a, VarL('a)), LetV('y, App(VarL('x), Char('b)), App(VarL('plus), Abs('f, TNum, VarL('y)))))))

  typecheckTest("List(1, 2, 3)", ListL(Num(1), Num(2), Num(3)))(ListT(Some(TNum)))
  typecheckTestError("List(1, 2, 'a)", ListL(Num(1), Num(2), Char('a)))
  typecheckTest("Let x = List(1, 2, 3) in x ", LetV('x, ListL(Num(0), Num(1), Num(2)), VarL('x)))(ListT(Some(TNum)))
  typecheckTest("Let l = List('a,'b) in let x = 'c in append l x", LetV('l, ListL(Char('a), Char('b)), LetV('x, Char('c), AppendE(VarL('l), VarL('x)))))(ListT(Some(TChar)))
  typecheckTestError("Let l = List('a,'b) in let x = 1 in append l x", LetV('l, ListL(Char('a), Char('b)), LetV('x, Num(1), AppendE(VarL('l), VarL('x)))))
  typecheckTest("List(1, 2, 3) ++ List(4, 5)", ++(ListL(Num(1), Num(2), Num(3)), ListL(Num(4), Num(5))))(ListT(Some(TNum)))
  typecheckTestError("List(1, 2, 3) ++ List(a, b)", ++(ListL(Num(1), Num(2), Num(3)), ListL(Char('a), Char('b))))
  typecheckTest("head(List(1, 2, 3))", Head(ListL(Num(1), Num(2), Num(3))))(TNum)
//  typecheckTestError("head(List())", Head(ListL()))
  typecheckTest("last(List(1, 2, 3))", Last(ListL(Num(1), Num(2), Num(3))))(TNum)
  typecheckTestError("last(1, 2, 'a)", Last((ListL(Num(1), Num(2), Char('a)))))
 // typecheckTestError("last(List())", Last(ListL()))
  typecheckTest("tail(List(1, 2, 3)", Tail(ListL(Num(1), Num(2), Num(3))))(ListT(Some(TNum)))
  typecheckTest("tail(List('a, 'b)", Tail(ListL(Char('a), Char('b))))(ListT(Some(TChar)))
  typecheckTestError("tail(List(1, 'a, 'b)", Tail(ListL(Num(1), Char('a), Char('b))))
  typecheckTest("[] ++ []", ++(ListL(), ListL()))(ListT(None))
  typecheckTest("[] ++ [1, 2, 3]", ++(ListL(), ListL(Num(1), Num(2), Num(3))))(ListT(Some(TNum)))
  typecheckTest("[1, 2, 3] ++ []", ++(ListL(Num(1), Num(2), Num(3)), ListL()))(ListT(Some(TNum)))

  typecheckTest("LetRec sum =  \\l. if l.isEmpty 0 else last l + sum  (init l ) in sum List(1, 2, 3)", LetRec('sum,
    Abs('l, IfElse(Bool('f), Num(0), Add(Last(Var('l)), App(Var('sum), Init(Var('l)))))), App(VarL('sum), ListL(Num(0), Num(1), Num(2))) ))(TNum)


 //++ Append

  typecheckTest("Append - LetRec ++ = \\l1\\l2. Match l1 (l2) \\h\\t. h +: App(App(++, t), l2) in ++ ",
    LetRec('++, Abs('l1, Abs('l2, Match(Var('l1), Var('l2), Abs('h, Abs('t, +:(Var('h), App(App(VarL('++), Var('t)), VarL('l2)))))))), VarL('++)))(TFun(ListT(Some(UVar(CVar('a)))), TFun(ListT(Some(UVar(CVar('a)))), ListT(Some(UVar(CVar('a)))))))

  typecheckTest("LetRec ++ = \\l1\\l2. Match l1 (l2) \\h\\t. h +: App(App(++, t), l2) in App(App(++, List(1, 2)), List(4, 5))",
    LetRec('++, Abs('l1, Abs('l2, Match(Var('l1), Var('l2), Abs('h, Abs('t, +:(Var('h), App(App(VarL('++), Var('t)), VarL('l2)))))))), App(App(VarL('++), ListL(Num(1), Num(2))), ListL(Num(4), Num(5))) ))(ListT(Some(TNum)))
  typecheckTest("LetRec ++ = \\l1\\l2. Match l1 (l2) \\h\\t. h +: App(App(++, t), l2) in App(App(++, List()), List(4, 5))",
    LetRec('++, Abs('l1, Abs('l2, Match(Var('l1), Var('l2), Abs('h, Abs('t, +:(Var('h), App(App(VarL('++), Var('t)), VarL('l2)))))))), App(App(VarL('++), ListL()), ListL(Num(4), Num(5))) ))(ListT(Some(TNum)))
  typecheckTest("LetRec ++ = \\l1\\l2. Match l1 (l2) \\h\\t. h +: App(App(++, t), l2) in App(App(++, List(1,2)), List())",
    LetRec('++, Abs('l1, Abs('l2, Match(Var('l1), Var('l2), Abs('h, Abs('t, +:(Var('h), App(App(VarL('++), Var('t)), VarL('l2)))))))), App(App(VarL('++), ListL(Num(1), Num(2))), ListL())) )(ListT(Some(TNum)))
  typecheckTest("LetRec ++ = \\l1\\l2. Match l1 (l2) \\h\\t. h +: App(App(++, t), l2) in App(App(++, List()), List())",
    LetRec('++, Abs('l1, Abs('l2, Match(Var('l1), Var('l2), Abs('h, Abs('t, +:(Var('h), App(App(VarL('++), Var('t)), VarL('l2)))))))), App(App(VarL('++), ListL()), ListL())) )(ListT(None))
  typecheckTestError("LetRec ++ = \\l1\\l2. Match l1 (l2) \\h\\t. h +: App(App(++, t), l2) in App(App(++, List(1, 2)), List(a, b))",
    LetRec('++, Abs('l1, Abs('l2, Match(Var('l1), Var('l2), Abs('h, Abs('t, +:(Var('h), App(App(VarL('++), Var('t)), VarL('l2)))))))),
      App(App(VarL('++), ListL(Num(1), Num(2))), ListL(Char('a), Char('b)) )))

  typecheckTest("LetRec ++ = \\l1\\l2. Match l1 (l2) \\h\\t. h +: App(App(++, t), l2) in tuple(App(App(++, List(1,2)), List(3,4)), App(App(++, List(a, b)), List(c, d)))",
    LetRec('++, Abs('l1, Abs('l2, Match(Var('l1), Var('l2), Abs('h, Abs('t, +:(Var('h), App(App(VarL('++), Var('t)), VarL('l2)))))))),
      TupleE(App(App(VarL('++), ListL(Num(1), Num(2))), ListL(Num(3), Num(4))), App(App(VarL('++), ListL(Char('a), Char('b))), ListL(Char('c), Char('d))))))(TupleL(ListT(Some(TNum)), ListT(Some(TChar))))

 //Head

  typecheckTest("head - Let head = \\ e.match e error(Empty list) \\h.\\t.h in head", LetV('head, Abs('e, Match(Var('e), Error("Empty List"), Abs('h, Abs('t, Var('h))))), VarL('head)))(TFun(ListT(Some(UVar(CVar('a)))), UVar(CVar('a))))

  typecheckTest("Let head = \\ e.match e error(Empty list) \\h.\\t.h in head(List(1, 2, 3))", LetV('head, Abs('e, Match(Var('e), Error("Empty List"), Abs('h, Abs('t, Var('h))))), App(VarL('head), ListL(Num(1), Num(2)))))(TNum)
  typecheckTest("Let head = \\ e.match e error(Empty list) \\h.\\t.h in head(List())", LetV('head, Abs('e, Match(Var('e), Error("Empty List"), Abs('h, Abs('t, Var('h))))), App(VarL('head), ListL())))(UVar(CVar('a)))

  typecheckTest("Let head = \\ e.match e error(Empty list) \\h.\\t.h in Tuple(head(List(1, 2, 3)), head(List['a,'b,'c]",
    LetV('head, Abs('e, Match(Var('e), Error("Empty List"), Abs('h, Abs('t, Var('h))))), TupleE(App(VarL('head), ListL(Num(1), Num(2))), App(VarL('head), ListL(Char('a), Char('b))))))(TupleL(TNum, TChar))

  //uncons

  typecheckTest("uncons - Let uncons = \\ e. match e List() \\h\\t. in uncons ",
    LetV('uncons, Abs('e, Match(Var('e), ListL(), Abs('h, Abs('t, TupleE(Var('h), Var('t)))))), VarL('uncons)))(TFun(ListT(Some(UVar(CVar('a)))), TupleL(UVar(CVar('a)), ListT(Some(UVar(CVar('a)))))))

  typecheckTest("Let uncons = \\ e. match e List() \\h\\t. in uncons List (1, 2, 3)",
    LetV('uncons, Abs('e, Match(Var('e), ListL(), Abs('h, Abs('t, TupleE(Var('h), Var('t)))))), App(VarL('uncons), ListL(Num(1), Num(2), Num(3)))))(TupleL(TNum, ListT(Some(TNum))))

    //tail

  typecheckTest("Tail - Let tail =  \\e. match e (error(Empty list tail )) (\\h. \\t. t)  in tail",
    LetV('tail, Abs('e, Match(Var('e), Error("Empty List tail"),  Abs('h, Abs('t, Var('t))))), VarL('tail)))(TFun(ListT(Some(UVar(CVar('a)))), ListT(Some(UVar(CVar('a))))))

  typecheckTest("Let tail =  \\e. match e (error(Empty list tail )) (\\h. \\t. t)  in tail List(1, 2, 3)",
    LetV('tail, Abs('e, Match(Var('e), Error("Empty List tail"),  Abs('h, Abs('t, Var('t))))), App(VarL('tail), ListL(Num(1), Num(2), Num(3)))))(ListT(Some(TNum)))

  typecheckTest("Let tail =  \\e. match e (error(Empty list tail )) (\\h. \\t. t)  in Tuplr(tail List(1, 2, 3), tail List(a, b, c))",
    LetV('tail, Abs('e, Match(Var('e), Error("Empty List tail"),  Abs('h, Abs('t, Var('t))))),
      TupleE(App(VarL('tail), ListL(Num(1), Num(2), Num(3))), App(VarL('tail), ListL(Char('a), Char('b), Char('c))))))(TupleL(ListT(Some(TNum)), ListT(Some(TChar))))

  //last

  typecheckTest("last - LetRec last = \\e. matchP e (\\h. h) (\\h .\\t. last t) (error(Empty List last) in last",
    LetRec('last, Abs('e, MatchP(Var('e), Abs('h, Var('h)), Abs('h, Abs('t, App(Var('last), Var('t)))), Error("Empty List last"))), VarL('last)))(TFun(ListT(Some(UVar(CVar('a)))), UVar(CVar('a))))

  typecheckTest("LetRec last = \\e. matchP e (\\h. h) (\\h .\\t. last t) (error(Empty List last) in last List(1, 2, 3)",
    LetRec('last, Abs('e, MatchP(Var('e), Abs('h, Var('h)), Abs('h, Abs('t, App(Var('last), Var('t)))), Error("Empty List last"))), App(VarL('last), ListL(Num(1), Num(2), Num(3)))))(TNum)

  //init
  typecheckTest("init - LetRec init = \\e. matchP e (\\h. List(h)) (\\h \\t init t) (error(Empty List init) in init",
    LetRec('init, Abs('e, MatchP(Var('e), Abs('h, ListL(Var('h))), Abs('h, Abs('t, App(Var('init), Var('t)))), Error("Empty List init"))), VarL('init)))(TFun(ListT(Some(UVar(CVar('a)))), ListT(Some(UVar(CVar('a))))))

  typecheckTest("LetRec init = \\e. matchP e (\\h. List(h)) (\\h \\t init t) (error(Empty List init) in init List(1, 2, 3)",
    LetRec('init, Abs('e, MatchP(Var('e), Abs('h, ListL(Var('h))), Abs('h, Abs('t, App(Var('init), Var('t)))), Error("Empty List init"))), App(VarL('init), ListL(Num(1), Num(2), Num(3)))))(ListT(Some(TNum)))

  //null
  typecheckTest("Let null = \\e match e True \\h\\t. False in null(List(1, 2, 3))",
    LetV('null, Abs('e, Match(Var('e), True(), Abs('h, Abs('t, False())))), App(VarL('null), ListL(Num(1), Num(2), Num(3)))))(TBool)

  //length
  typecheckTest("length - LetRec lenAcc = \\e. \\n.  match e  (n) (\\h\\t. App (App lenAcc t) (n + 1)) in Let length = \\e. App (App lenAcc e) 0 in length",
    LetRec('lenAcc, Abs('e, Abs('n, Match(Var('e), Var('n), Abs('h, Abs('t, App(App(Var('lenAcc), Var('t)), Add(Var('n), Num(1)))))))),
      LetV('length, Abs('e, App(App(VarL('lenAcc), Var('e)), Num(0))), VarL('length))))(TFun(ListT(Some(UVar(CVar('a)))), TNum))

  typecheckTest("LetRec lenAcc = \\e. \\n.  match e  (n) (\\h\\t. App (App lenAcc t) (n + 1)) in Let length = \\e. App (App lenAcc e) 0 in length List(a, b)",
    LetRec('lenAcc, Abs('e, Abs('n, Match(Var('e), Var('n), Abs('h, Abs('t, App(App(Var('lenAcc), Var('t)), Add(Var('n), Num(1)))))))),
      LetV('length, Abs('e, App(App(VarL('lenAcc), Var('e)), Num(0))), App(VarL('length), ListL(Char('a), Char('b))) )))(TNum)

  typecheckTest("LetRec lenAcc = \\e. \\n.  match e  (n) (\\h\\t. App (App lenAcc t) (n + 1)) in Let length = \\e. App (App lenAcc e) 0 in Add(length List(a, b), length List(1, 2, 3))",
    LetRec('lenAcc, Abs('e, Abs('n, Match(Var('e), Var('n), Abs('h, Abs('t, App(App(Var('lenAcc), Var('t)), Add(Var('n), Num(1)))))))),
      LetV('length, Abs('e, App(App(VarL('lenAcc), Var('e)), Num(0))), Add(App(VarL('length), ListL(Char('a), Char('b))), App(VarL('length), ListL(Num(1), Num(2), Num(3)))) )))(TNum)

  //map
  typecheckTest(" LetRec map = \\f\\l.Match l (List()) \\h\\t. f h : map f xs in map ",
      LetRec('map, Abs('f, Abs('l, Match(Var('l), ListL(), Abs('h, Abs('t, +:(App(Var('f), Var('h)), App(App(VarL('map), Var('f)), Var('t)))))))), VarL('map)))(TFun(TFun(UVar(CVar('a)),UVar(CVar('b))),TFun(ListT(Some(UVar(CVar('a)))), ListT(Some(UVar(CVar('b)))))))

  typecheckTest("Let add1 = \\n. 1 + n in LetRec map = \\f\\l.Match l (List()) \\h\\t. f h : map f xs in map add1: Nm -> Num List(1, 2, 3)",
    LetV('add1, Abs('n, Add(Var('n), Num(1))),
      LetRec('map, Abs('f, Abs('l, Match(Var('l), ListL(), Abs('h, Abs('t, +:(App(Var('f), Var('h)), App(App(VarL('map), Var('f)), Var('t)))))))), App(App(VarL('map),VarL('add1)), ListL(Num(1), Num(2), Num(3))))))(ListT(Some(TNum)))

  typecheckTestError("Let add1 = \\n. 1 + n in LetRec map = \\f\\l.Match l (List()) \\h\\t. f h : map f xs in map add1: Nm -> Num List(a, b)",
    LetV('add1, Abs('n, Add(Var('n), Num(1))),
      LetRec('map, Abs('f, Abs('l, Match(Var('l), ListL(), Abs('h, Abs('t, +:(App(Var('f), VarL('h)), App(App(Var('map), Var('f)), Var('t)))))))),
        App(App(VarL('map),VarL('add1)), ListL(Char('a), Char('b))))))

  typecheckTest("Let add1 = \\n. 1 + n in LetRec map = \\f\\l.Match l (List()) \\h\\t. f h : map f xs in tuple(map add1: Nm -> Num List(1, 2, 3), map appA a -> bool List(a, b, c)",
    LetV('add1, Abs('n, Add(Var('n), Num(1))), LetV('isLow, Abs('e, IsLower(Var('e))),
      LetRec('map, Abs('f, Abs('l, Match(Var('l), ListL(), Abs('h, Abs('t, +:(App(Var('f), Var('h)), App(App(VarL('map), Var('f)), Var('t)))))))),
        TupleE(App(App(VarL('map),VarL('add1)), ListL(Num(1), Num(2), Num(3))), App(App(VarL('map),VarL('isLow)), ListL(Char('a), Char('b), Char('c))))))))(TupleL(ListT(Some(TNum)), ListT(Some(TBool))))

  //reverse
  typecheckTest("reverse - LetRec rev = \\l. \\e. Match l Var(e)  \\h. \\t. rev t (x : e) in Let reverse = \\l. rev l []  in reverse ",
    LetRec('rev, Abs('l, Abs('e , Match(Var('l), Var('e), Abs('h, Abs('t, App(App(VarL('rev), Var('t)), +: (Var('h), Var('e)))))))),
      LetV('reverse, Abs('l, App(App(VarL('rev), VarL('l)), ListL())), VarL('reverse))))(TFun(ListT(Some(UVar(CVar('a)))), ListT(Some(UVar(CVar('a))))))

  typecheckTest("LetRec rev = \\l. \\e. Match l Var(e)  \\h. \\t. rev t (x : e) in Let reverse = \\l. rev l []  in reverse List[1, 2, 3]",
    LetRec('rev, Abs('l, Abs('e , Match(Var('l), Var('e), Abs('h, Abs('t, App(App(VarL('rev), Var('t)), +: (Var('h), Var('e)))))))),
      LetV('reverse, Abs('l, App(App(VarL('rev), VarL('l)), ListL())), App(VarL('reverse), ListL(Num(1), Num(2), Num(3))))))(ListT(Some(TNum)))

  typecheckTest("LetRec rev = \\l. \\e. Match l Var(e)  \\h. \\t. rev t (x : e) in Let reverse = \\l. rev l []  in Tuple(reverse List[1, 2, 3], reverse List['a, 'b, 'c]",
    LetRec('rev, Abs('l, Abs('e , Match(Var('l), Var('e), Abs('h, Abs('t, App(App(VarL('rev), Var('t)), +: (Var('h), Var('e)))))))),
      LetV('reverse, Abs('l, App(App(VarL('rev), VarL('l)), ListL())),
        TupleE(App(VarL('reverse), ListL(Num(1), Num(2), Num(3))), App(VarL('reverse), ListL(Char('a), Char('b), Char('c)))))))(TupleL(ListT(Some(TNum)), ListT(Some(TChar))))


  //and
  typecheckTest("LetRec and = \\l. Match l True \\h. \\t. h && and t in List(true, false, true",
    LetRec('and, Abs('l, Match(Var('l), True(), Abs('h, Abs('t, &&(Var('h), App(VarL('and), Var('t))))))), App(VarL('and), ListL(True(), False(), True()))))(TBool)

  //or
  typecheckTest("LetRec or = \\l. Match l True \\h. \\t. h || and t in or :: List(bool) -> bool",
    LetRec('or, Abs('l, Match(Var('l), True(), Abs('h, Abs('t, ||(Var('h), App(VarL('or), Var('t))))))), App(VarL('or), ListL(True(), False(), True()))))(TBool)



  //  typecheckTest("LetRec init' = \\ x \\ xs. match xs List() \\y \\zs init' y zs in Let init = \\ e. match e error(Empty list) \\h \\t. init' h t in init List (1, 2, 3)",
//    LetRec('initP, Abs('x, Abs('xs, Match(Var('xs), ListL(), Abs('y, Abs('zs, App(Var('initP), Abs('y, Var('zs))))) ))),
//      LetV('init, Abs('e, Match(Var('e), Error("Empty List"), Abs('h, Abs('t, App(VarL('initP), Abs('h, Var('t))))))), App(VarL('init), ListL(Num(1), Num(2), Num(3))))))(ListT(Some(TNum)))

}

class TestBUSolveEndCorrectness extends TestCorrectness("BUSolveEnd", new BUCheckerFactory(SolveEnd))
class TestBUSolveContinuousSubstCorrectness extends TestCorrectness("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))
class TestBUSolveContinuouslyCorrectness extends TestCorrectness("BUSolveContinuously", new BUCheckerFactory(SolveContinuously))
