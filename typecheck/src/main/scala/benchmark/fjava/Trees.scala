package benchmark.fjava

import incremental.Node._
import incremental.fjava._
import org.scalameter.api.Gen
import org.scalameter.picklers.Implicits.intPickler

import Predef.{$conforms => _, _}

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

  type MkClassWrap = Node => Node

  /**
    * @return true if the path refers to a root class (which subclasses Object)
    */
  def isRoot(path: Seq[Int]) = path.size == 1

  /**
    * @return optionally the path to the same class in the previous subhierarchy root
    */
  def prevRoot(path: Seq[Int]): Option[Seq[Int]] = if (path.head == 0) None else Some(path.updated(0, path.head - 1))

  object Config {
    def fromValue(n: Int) = n match {
      case 0 => Unique
      case 1 => Mirrored
      case 2 => Overriding
      case 3 => MirroredOverriding
    }
  }
  sealed class Config(val ignoreRoot: Boolean, val ignorePath: Boolean, val desc: String, val value: Int) extends Serializable {
    override def toString: String = desc
  }
  object Unique extends Config(false, false, "unique", 0)
  object Mirrored extends Config(true, false, "mirrored", 1)
  object Overriding extends Config(false, true, "overriding", 2)
  object MirroredOverriding extends Config(true, true, "mirrored+overriding", 3)


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
  def hierarchy(roots: Int, height: Int, branching: Int)(implicit mkFields: MkFields, mkMethods: MkMethods, mkClassWrap: MkClassWrap): Node = {
    val rootClasses = for (rootID <- 0 until roots)
      yield subhierarchy(height, branching, Seq(rootID))
    ProgramM(Seq(), rootClasses)
  }

  /**
    * @param height height of each subhierarchy
    * @param branching subclass branching factor within subhierarchies
    * @param path path to current subhierarchy
    */
  def subhierarchy(height: Int, branching: Int, path: Seq[Int])(implicit mkFields: MkFields, mkMethods: MkMethods, mkClassWrap: MkClassWrap): Node = {
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
    val cls = mkClassWrap(ClassDec(
      Seq(cname(path), csuper(path), Ctor(superfields, fields), fields.toSeq),
      mkMethods(path)
    ))

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
  val noClassWrap: MkClassWrap = cls => cls

  /**
    * Generates a class hierarchy where
    *  - each class has a single field of type Nat
    *  - each class has a single method that calls the corresponding method
    *    in the superclass and adds its field value to the result
    *  - all fields and methods have distinct names (no overriding)
    */
  def intAccumSuperHierarchy(roots: Int, height: Int, branching: Int)(implicit config: Config): Node = {
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

    implicit val mkClassWrap = noClassWrap

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

    implicit val mkClassWrap = noClassWrap

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
  def intAccumPrevSuperHierarchy(roots: Int, height: Int, branching: Int)(implicit config: Config): Node = {
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

    implicit val mkClassWrap: MkClassWrap = noClassWrap // cls => ProgramM(cls, Classes.NatClasses)

    val prog = hierarchy(roots, height, branching)
    ProgramM(prog, Classes.NatClasses.cloned)
  }


  val rootss = Gen.enumeration("roots")(10, 20, 40)
  val heightss = Gen.enumeration("heights")(5)
  val branchings = Gen.enumeration("branching")(2)
  val configs = Gen.enumeration[Int]("naming")(Unique.value, Mirrored.value, Overriding.value, MirroredOverriding.value)

  val intAccumSuperHierarchyTrees = for
      { roots <- rootss
        heights <- heightss
        branching <- branchings
        configValue <- configs;
        config = Config.fromValue(configValue) }
    yield intAccumSuperHierarchy(roots, heights, branching)(config) -> hierarchySize(roots, heights, branching)

  val intAccumPrevHierarchyTrees = for
      { roots <- rootss
        heights <- heightss
        branching <- branchings
        configValue <- configs;
        config = Config.fromValue(configValue) }
  yield intAcumPrevHierarchy(roots, heights, branching)(config) -> hierarchySize(roots, heights, branching)

  val intAccumPrevSuperHierarchyTrees = for
      { roots <- rootss
        heights <- heightss
        branching <- branchings
        configValue <- configs;
        config = Config.fromValue(configValue) }
    yield intAccumPrevSuperHierarchy(roots, heights, branching)(config) -> hierarchySize(roots, heights, branching)
}
