package benchmark.fjava

import incremental.Node._
import incremental.fjava._

import scala.collection.immutable.ListMap

object Classes {
  val Nat = ClassDec(
    Seq(CName('Nat), CName('Object),  Ctor(ListMap(), ListMap()),
      Seq()), // no fields
    Seq(
      MethodDec(
        CName('Nat), 'succ, Seq(),
        Var('this)), // dummy body, will be overwritten by subclasses
      MethodDec(
        CName('Nat), 'pred, Seq(),
        Var('this)), // dummy body, will be overwritten by subclasses
      MethodDec(
        CName('Nat), 'plus, Seq('other -> CName('Nat)),
        Var('this )) // dummy body, will be overwritten by subclasses
    )
  )

  val Zero = ClassDec(
    Seq(CName('Zero), CName('Nat),  Ctor(ListMap(), ListMap()),
      Seq()), // no fields
    Seq(
      MethodDec(
        CName('Nat), 'succ, Seq(),
        Var('this)),
      MethodDec(
        CName('Nat), 'pred, Seq(),
        Var('this)), // pred of Zero is Zero
      MethodDec(
        CName('Nat), 'plus, Seq('other -> CName('Nat)),
        Var('other))
    )
  )

  val Succ = ClassDec(
    Seq(CName('Succ), CName('Nat),  Ctor(ListMap(), ListMap('x -> CName('Nat))),
      Seq(('x -> CName('Nat)))), // x is the predecessor of this nat
    Seq(
      MethodDec(
        CName('Nat), 'succ, Seq(),
        New(CName('Succ), New(CName('Nat)))), // --- New(CName('Succ), Var('this))),
      MethodDec(
        CName('Nat), 'pred, Seq(),
        FieldAcc('x, Var('this))), // pred of Zero is Zero -- FieldAcc('x, New(CName('Succ), Var('this))))
      MethodDec(
        CName('Nat), 'plus, Seq('other -> CName('Nat)),
        New(CName('Succ), Invk('plus, FieldAcc('x, Var('this)), Var('other)))) // plus(Succ(x), other) = Succ(plus(x, other))
    )
  )

  val NatClasses = Seq(Nat, Zero, Succ)





  val Title = ClassDec(
    Seq(CName('Title), CName('Object), Ctor(ListMap(), ListMap()),
      Seq()), // no fields
    Seq() // no methods
  )
  val NoTitle = ClassDec(
    Seq(CName('NoTitle), CName('Title),  Ctor(ListMap(), ListMap()),
      Seq()), // no fields
    Seq() // no methods
  )
  val ProfTitle = ClassDec(
    Seq(CName('ProfTitle), CName('Title), Ctor(ListMap(), ListMap()),
      Seq()), // no fields
    Seq() // no methods
  )

  val Person = ClassDec(
    Seq(CName('Person), CName('Object),  Ctor(ListMap(), ListMap('name -> CName('Object), 'address -> CName('Object))),
      Seq(
        ('name -> CName('Object)),
        ('address -> CName('Object)))),
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
    Seq(CName('Professor), CName('Person), Ctor(ListMap('name -> CName('Object), 'address -> CName('Object)), ListMap('age -> CName('Object))),
      Seq(('age -> CName('Object)))),
    Seq(
      MethodDec(
        CName('Object), 'age, Seq(),
        FieldAcc('age, Var('this))),
      MethodDec(
        CName('Title), 'title, Seq(),
        New(CName('ProfTitle)))
    )
  )
  val Student = ClassDec(
    Seq(CName('Student), CName('Person), Ctor(ListMap('name -> CName('Object), 'address -> CName('Object)), ListMap('age -> CName('Object))),
      Seq(('age -> CName('Object)))), // no fields
    Seq(
      MethodDec(
        CName('Object), 'age, Seq(),
        FieldAcc('age, Var('this))),
      MethodDec(
        CName('Title), 'title, Seq(),
        New(CName('NoTitle))),
      MethodDec(
        CName('Professor), 'promote, Seq(),
        New(CName('Professor), // copy name, address, and age
          FieldAcc('name, Var('this)),
          FieldAcc('address, Var('this)),
          FieldAcc('age, Var('this))))
    )
  )
  val PersonClasses = Seq(ProfTitle, Professor, Person, Title, Student, NoTitle)
}