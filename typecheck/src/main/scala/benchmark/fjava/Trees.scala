package benchmark.fjava

import incremental.Node._
import incremental.fjava._

import scala.collection.immutable.ListMap

/**
  * Created by seba on 20.12.16.
  */
object Trees {

  /**
    * @param path path to current subhierarchy
    */
  type MkFields = Seq[Int] => ListMap[Symbol, CName]

  /**
    * @param path path to current subhierarchy
    */
  type MkMethods = Seq[Int] => Seq[Node]

  /**
    * @return true if the path refers to a root class (which subclasses Object)
    */
  def isRoot(path: Seq[Int]) = path.size == 1

  /**
    * @return optionally the path to the same class in the previous subhierarchy root
    */
  def prevRoot(path: Seq[Int]): Option[Seq[Int]] = if (path.head == 0) None else Some(path.updated(0, path.head - 1))

  case class Config(ignoreRoot: Boolean, ignorePath: Boolean)

  /**
    * @param roots number of subhierarchies below Object
    * @param height height of each subhierarchy
    * @param branching subclass branching factor within subhierarchies
    */
  def hierarchySize(roots: Int, height: Int, branching: Int) =
    roots * (Math.pow(branching, height) - 1)

  /**
    * @param roots number of subhierarchies below Object
    * @param height height of each subhierarchy
    * @param branching subclass branching factor within subhierarchies
    */
  def hierarchy(roots: Int, height: Int, branching: Int)(implicit mkFields: MkFields, mkMethods: MkMethods): Node = {
    val rootClasses = for (rootID <- 0 until roots)
      yield subhierarchy(height, branching, Seq(rootID))
    ProgramM(Seq(), rootClasses.flatten)
  }

  /**
    * @param height height of each subhierarchy
    * @param branching subclass branching factor within subhierarchies
    * @param path path to current subhierarchy
    */
  def subhierarchy(height: Int, branching: Int, path: Seq[Int])(implicit mkFields: MkFields, mkMethods: MkMethods): Seq[Node] = {
    val fields = mkFields(path)
    val superfields = if (isRoot(path)) ListMap[Symbol, CName]() else mkFields(path.dropRight(1))
    val cls = ClassDec(
      Seq(cname(path), csuper(path), Ctor(superfields, fields), fields.toSeq),
      mkMethods(path)
    )

    if (height == 1)
      Seq(cls)
    else {
      val branches = for (b <- 0 until branching) yield subhierarchy(height - 1, branching, path :+ b)
      cls +: branches.flatten
    }
  }

  def cname(path: Seq[Int]): CName =
    CName(Symbol(s"C-${string(path)(Config(false, false))}"))

  def csuper(path: Seq[Int]): CName =
    if (isRoot(path))
      CName('Object)
    else
      cname(path.dropRight(1))

  def string(path: Seq[Int])(implicit config: Config) = {
    var thepath = path.map(_.toString)
    if (config.ignoreRoot)
      thepath = thepath.updated(0, "_")
    if (config.ignorePath)
      thepath = thepath.take(1)
    thepath.mkString("[", ",", "]")
  }

  def fname(path: Seq[Int], suffix: String = "")(implicit config: Config) = {
    Symbol(s"f-${string(path)}$suffix")
  }
  def mname(path: Seq[Int], suffix: String = "")(implicit config: Config) =
    Symbol(s"m-${string(path)}$suffix")

  val noFields: MkFields = path => ListMap()
  val noMethods: MkMethods = path => Seq()

  /**
    * Generates a class hierarchy where
    *  - each class has a single field of type Nat
    *  - each class has a single method that calls the corresponding method
    *    in the superclass and adds its field value to the result
    *  - all fields and methods have distinct names (no overriding)
    */
  def intAcumSuperHierarchy(roots: Int, height: Int, branching: Int)(implicit config: Config): Node = {
    implicit val mkFields: MkFields = path =>
      ListMap(fname(path) -> CName('Nat))

    implicit val mkMethods: MkMethods = path => {
      val superAcum = if (isRoot(path)) New(CName('Zero)) else Invk(mname(path.dropRight(1)), Var('this))
      Seq(
        MethodDec(CName('Nat), mname(path), Seq(),
          Invk('plus, FieldAcc(fname(path), Var('this)), superAcum)
        )
      )
    }

    val prog = hierarchy(roots, height, branching)
    ProgramM(Seq(), prog.kids.seq ++ Classes.NatClasses)
  }


  /**
    * Generates a class hierarchy where
    *  - each class has a field of type Nat
    *  - each class has a field of its prev-root class
    *  - each class has a single method that calls the corresponding method
    *    in the prev-root class and adds its field value to the result
    *  - all fields and methods have distinct names (no overriding)
    */
  def intAcumPrevHierarchy(roots: Int, height: Int, branching: Int)(implicit config: Config): Node = {
    implicit val mkFields: MkFields = path => {
      val prevField = prevRoot(path) match {
        case None => Seq()
        case Some(prev) => Seq(fname(path, "-prev") -> cname(prev))
      }
      ListMap(fname(path) -> CName('Nat)) ++ prevField
    }

    implicit val mkMethods: MkMethods = path => {
      val prevAcum = prevRoot(path) match {
        case None => New(CName('Zero))
        case Some(prev) => Invk(mname(prev), FieldAcc(fname(path, "-prev"), Var('this)))
      }
      Seq(
        MethodDec(CName('Nat), mname(path), Seq(),
          Invk('plus, FieldAcc(fname(path), Var('this)), prevAcum)
        )
      )
    }

    val prog = hierarchy(roots, height, branching)
    ProgramM(Seq(), prog.kids.seq ++ Classes.NatClasses)
  }

  /**
    * Generates a class hierarchy where
    *  - each class has a field of type Nat
    *  - each class has a field of its prev-root class
    *  - each class has a single method that calls the corresponding method
    *    in the prev-root class and in the superclass and adds its field value to the result
    *  - all fields and methods have distinct names (no overriding)
    */
  def intAcumPrevSuperHierarchy(roots: Int, height: Int, branching: Int)(implicit config: Config): Node = {
    implicit val mkFields: MkFields = path => {
      val prevField = prevRoot(path) match {
        case None => Seq()
        case Some(prev) => Seq(fname(path, "-prev") -> cname(prev))
      }
      ListMap(fname(path) -> CName('Nat)) ++ prevField
    }

    implicit val mkMethods: MkMethods = path => {
      val prevAcum = prevRoot(path) match {
        case None => New(CName('Zero))
        case Some(prev) => Invk(mname(prev), FieldAcc(fname(path, "-prev"), Var('this)))
      }
      val superAcum = if (isRoot(path)) New(CName('Zero)) else Invk(mname(path.dropRight(1)), Var('this))
      Seq(
        MethodDec(CName('Nat), mname(path), Seq(),
          Invk('plus, Invk('plus, FieldAcc(fname(path), Var('this)), prevAcum), superAcum)
        )
      )
    }

    val prog = hierarchy(roots, height, branching)
    ProgramM(Seq(), prog.kids.seq ++ Classes.NatClasses)
  }


}
