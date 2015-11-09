package incremental.fjava

import constraints.fjava._
import constraints.fjava.impl._
import incremental.Node._
import org.scalatest.{BeforeAndAfterEach, FunSuite}

import scala.collection.immutable.ListMap

/**
 * Created by lirakuci on 3/29/15.
 */
class TestPerson[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: BUCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
  val checker: BUChecker[CS] = checkerFactory.makeChecker

  override def afterEach: Unit = checker.localState.printStatistics()

  def typecheckTest(desc: String, e: => Node)(expected: Type): Unit =
    test(s"$classdesc: Type check $desc") {
      val ev = e
      val actual = checker.typecheck(ev)

      val typ = ev.withType[checker.Result].typ._1
      val req = ev.withType[checker.Result].typ._2
      val creq = ev.withType[checker.Result].typ._3
      val cons = ev.withType[checker.Result].typ._4
      assert(actual.isLeft, s"Expected $expected but got Type = $typ, Reqs = $req, CReqs = $creq, Constraint = $cons")

      val sol = SolveContinuousSubst.state.withValue(checker.csFactory.state.value) {
        Equal(expected, actual.left.get).solve(SolveContinuousSubst.freshConstraintSystem).tryFinalize
      }
      assert(sol.isSolved, s"Expected $expected but got ${actual.left.get}. Match failed with ${sol.unsolved}")
    }

  def typecheckTestError(desc: String, e: => Node) =
    test(s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }




  val Title = ClassDec(
    Seq(CName('Title), CName('Object), Ctor(ListMap(), List(), ListMap()),
      Seq()), // no fields
    Seq() // no methods
  )
  val NoTitle = ClassDec(
    Seq(CName('NoTitle), CName('Title),  Ctor(ListMap(), List(), ListMap()),
      Seq()), // no fields
    Seq() // no methods
  )
  val ProfTitle = ClassDec(
    Seq(CName('ProfTitle), CName('Title), Ctor(ListMap(), List(), ListMap()),
      Seq()), // no fields
    Seq() // no methods
  )

  val Person = ClassDec(
    Seq(CName('Person), CName('Object),  Ctor(ListMap('name -> CName('Object),
      'address -> CName('Object)), List(), ListMap('name -> 'name, 'address -> 'address)),
      Seq(
        'name -> CName('Object),
        'address -> CName('Object))),
    Seq(
      MethodDec(
        CName('Object), 'age, Seq(),
        Var('this)), // dummy body, will be overwritten by subclasses
      MethodDec(
        CName('Title), 'title, Seq(),
       New(CName('Title))) // dummy body, will be overwritten by subclasses  Var(This) ==> check agian the diffference bettwen them
    )
  )
  val Professor = ClassDec(
    Seq(CName('Professor), CName('Person), Ctor(ListMap('name -> CName('Object),
      'address -> CName('Object), 'age -> CName('Object)), List('name, 'address), ListMap('age->'age)),
      Seq('age -> CName('Object))), // no fields
    Seq(
      MethodDec(
        CName('Object), 'age, Seq(),
        Fields('age, Var('this))),
      MethodDec(
        CName('ProfTitle), 'title, Seq(),
        New(CName('ProfTitle)))
    )
  )
  val Student = ClassDec(
    Seq(CName('Student), CName('Person), Ctor(ListMap('name -> CName('Object),
      'address -> CName('Object), 'age -> CName('Object)), List('name, 'address), ListMap('age->'age)),
      Seq('age -> CName('Object))), // no fields
    Seq(
      MethodDec(
        CName('Object), 'age, Seq(),
        Fields('age, Var('this))),
      MethodDec(
        CName('NoTitle), 'title, Seq(),
        New(CName('NoTitle))),
      MethodDec(
        CName('Professor), 'promote, Seq(),
        New(CName('Professor), // copy name, address, and age
          Fields('name, Var('this)),
          Fields('address, Var('this)),
          Fields('age, Var('this))))
    )
  )

  typecheckTest("Title ok", Title)(CName('Title))
  typecheckTestError("NoTitle ok", NoTitle)
  typecheckTestError("ProfTitle ok", ProfTitle)

  typecheckTestError("Person ok", Person)
  typecheckTest("{Title, Person} ok", ProgramM(Title, Person))(CName('Object))
  typecheckTestError("Professor ok", Professor)
  typecheckTestError("Student ok", Student)

  typecheckTestError("{ProfTitle, Professor} ok", ProgramM(ProfTitle, Professor)) // misses superclass Person
  typecheckTestError("{ProfTitle, Professor, Person} ok", ProgramM(ProfTitle, Professor, Person)) // misses Title in Person.title()
  typecheckTest("{ProfTitle, Professor, Person, Title} ok", ProgramM(ProfTitle, Professor, Person, Title))(CName('Object))

  typecheckTestError("{ProfTitle, Professor, Person, Title, Student} ok", ProgramM(ProfTitle, Professor, Person, Title, Student)) // misses NoTitle
  typecheckTest("{ProfTitle, Professor, Person, Title, Student, NoTitle} ok", ProgramM(ProfTitle, Professor, Person, Title, Student, NoTitle))(CName('Object))
}

class TestBUSolveEndPerson extends TestPerson("BUSolveEnd", new BUCheckerFactory(SolveContinuousSubst))
//class TestBUSolveContinuouslyCorrectness extends TestCorrectness("BUSolveContinuously", new BUCheckerFactory(SolveContinuously))
//class TestBUSolveContinuousSubstCorrectness extends TestCorrectness("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))
//class TestBUSolveContinuousSubstThresholdCorrectness extends TestCorrectness("BUSolveContinuousSubstThreshold", new BUCheckerFactory(SolveContinuousSubstThreshold))

