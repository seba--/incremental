package benchmark.fjava

import incremental.Node._
import incremental.fjava._

import scala.collection.immutable.ListMap

/**
  * Created by seba on 20.12.16.
  */
object Trees {

  /**
    * @param rootID ID of the current root
    * @param path path to current subhierarchy
    */
  type MkFields = (Int, Seq[Int]) => ListMap[Symbol, CName]

  /**
    * @param rootID ID of the current root
    * @param path path to current subhierarchy
    */
  type MkMethods = (Int, Seq[Int]) => Seq[Node]

  val noFields: MkFields = (rootID, path) => ListMap()
  val noMethods: MkMethods = (rootID, path) => Seq()

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
  def hierarchy(roots: Int, height: Int, branching: Int)(implicit mkFields: MkFields, mkMethods: MkMethods): Seq[Node] = {
    val rootClasses = for (rootID <- 0 until roots)
      yield subhierarchy(height, branching, rootID)
    rootClasses.flatten
  }

  /**
    * @param rootID ID of the current root
    * @param height height of each subhierarchy
    * @param branching subclass branching factor within subhierarchies
    * @param path path to current subhierarchy
    */
  def subhierarchy(height: Int, branching: Int, rootID: Int, path: Seq[Int] = Seq())(implicit mkFields: MkFields, mkMethods: MkMethods): Seq[Node] = {
    val fields = mkFields(rootID, path)
    val superfields = if (path.isEmpty) ListMap[Symbol, CName]() else mkFields(rootID, path.dropRight(1))
    val cls = ClassDec(
      Seq(cname(rootID, path), csuper(rootID, path), Ctor(superfields, fields), fields.toSeq),
      mkMethods(rootID, path)
    )

    if (height == 1)
      Seq(cls)
    else {
      val branches = for (b <- 0 until branching) yield subhierarchy(height - 1, branching, rootID, path :+ b)
      cls +: branches.flatten
    }
  }

  def cname(rootID: Int, path: Seq[Int]): CName =
    if (path.isEmpty)
      CName(Symbol(s"C-root$rootID"))
    else
      CName(Symbol(s"C-root$rootID-${path.mkString("[" , ",", "]")}"))

  def csuper(rootID: Int, path: Seq[Int]): CName =
    if (path.isEmpty)
      CName('Object)
    else
      cname(rootID, path.dropRight(1))
}
