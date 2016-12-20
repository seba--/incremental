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

}