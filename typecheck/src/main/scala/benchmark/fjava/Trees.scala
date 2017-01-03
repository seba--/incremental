package benchmark.fjava

import incremental.Node._
import incremental.fjava._
import org.scalameter.api.Gen
import org.scalameter.picklers.Implicits.intPickler
import org.scalameter.picklers.Pickler

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

  sealed class Config(val ignoreRoot: Boolean, val ignorePath: Boolean, val desc: String)
  object Unique extends Config(false, false, "unique")
  object Mirrored extends Config(true, false, "mirrored")
  object Overriding extends Config(false, true, "overriding")
  object MirroredOverriding extends Config(true, true, "mirrored+overriding")


  implicit val configPickler = new Pickler[Config] {
    override def pickle(x: Config): Array[Byte] = x match {
      case Unique => intPickler.pickle(0)
      case `Mirrored` => intPickler.pickle(1)
      case `Overriding` => intPickler.pickle(2)
      case `MirroredOverriding` => intPickler.pickle(3)
      case _ => throw new MatchError(x)
    }

    override def unpickle(a: Array[Byte], from: Int): (Config, Int) = intPickler.unpickle(a, from) match {
      case (0, from) => (Unique, from)
      case (1, from) => (Mirrored, from)
      case (2, from) => (Overriding, from)
      case (3, from) => (MirroredOverriding, from)
    }
  }

  /**
    * @param roots number of subhierarchies below Object
    * @param height height of each subhierarchy
    * @param branching subclass branching factor within subhierarchies
    */
  def hierarchySize(roots: Int, height: Int, branching: Int): Long =
    roots * (Math.pow(branching, height).toLong - 1)

  /**
    * @param roots number of subhierarchies below Object
    * @param height height of each subhierarchy
    * @param branching subclass branching factor within subhierarchies
    */
  def hierarchy(roots: Int, height: Int, branching: Int)(implicit mkFields: MkFields, mkMethods: MkMethods): Node = {
    val rootClasses = for (rootID <- 0 until roots)
      yield subhierarchy(height, branching, Seq(rootID))
    ProgramM(Seq(), rootClasses)
  }

  /**
    * @param height height of each subhierarchy
    * @param branching subclass branching factor within subhierarchies
    * @param path path to current subhierarchy
    */
  def subhierarchy(height: Int, branching: Int, path: Seq[Int])(implicit mkFields: MkFields, mkMethods: MkMethods): Node = {
    val fields = mkFields(path)
    val superfields = {
      var prefix = 1
      var fields = ListMap[Symbol, CName]()
      while (prefix < path.size) {
        val superPath = path.take(prefix)
        fields = fields ++ mkFields(superPath)
        prefix += 1
      }
      fields
    }
    val cls = ClassDec(
      Seq(cname(path), csuper(path), Ctor(superfields, fields), fields.toSeq),
      mkMethods(path)
    )

    if (height < 1)
      ProgramM()
    else if (height == 1)
      cls
    else {
      val branches = for (b <- 0 until branching) yield subhierarchy(height - 1, branching, path :+ b)
      ProgramM(Seq(), branches :+ cls)
    }
  }

  def cname(path: Seq[Int]): CName =
    CName(Symbol(s"C-${string(path)(Unique)}"))

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
    ProgramM(prog, Classes.NatClasses)
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
    ProgramM(prog, Classes.NatClasses)
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
    ProgramM(prog, Classes.NatClasses)
  }


  val rootss = Gen.enumeration("roots")(10)//(10, 100)
  val heightss = Gen.enumeration("heights")(5)//(5, 10)
  val branchings = Gen.single("branching")(2)
  val configs = Gen.enumeration[Config]("naming")(Unique)//(Unique, Mirrored, Overriding, MirroredOverriding)

  val intAcumSuperHierarchyTrees = for
      { roots <- rootss
        heights <- heightss
        branching <- branchings
        config <- configs }
    yield intAcumSuperHierarchy(roots, heights, branching)(config) -> hierarchySize(roots, heights, branching)

  val intAcumPrevHierarchyTrees = for
      { roots <- rootss
        heights <- heightss
        branching <- branchings
        config <- configs }
  yield intAcumPrevHierarchy(roots, heights, branching)(config) -> hierarchySize(roots, heights, branching)

  val intAcumPrevSuperHierarchyTrees = for
      { roots <- rootss
        heights <- heightss
        branching <- branchings
        config <- configs }
    yield intAcumPrevSuperHierarchy(roots, heights, branching)(config) -> hierarchySize(roots, heights, branching)
}
