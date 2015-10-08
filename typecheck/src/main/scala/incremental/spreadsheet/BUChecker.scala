package incremental.spreadsheet

import constraints.Statistics
import constraints.equality._
import incremental.{Node_, Util}
import incremental.Node.Node

abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {
  import csFactory._

  type TError = String

  // TODO: SheetName -> (RowNum -> (ColName -> Type))
  // RowNum -> (ColName -> Type)
  type Reqs = Map[Int, Map[String, Type]]

  type StepResult = (Type, Reqs, Seq[Constraint])
  type Result = (Type, Reqs, CS)

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    Util.timed(localState -> Statistics.typecheckTime) {
      root.visitUninitialized {e =>
        typecheckRec(e)
        true
      }

      val (t_, reqs, sol_) = root.typ
      val sol = sol_.tryFinalize
      val t = t_.subst(sol.substitution)

      if (!reqs.isEmpty)
        Right(s"Unresolved context requirements $reqs, type $t, unres ${sol.unsolved}")
      else if (!sol.isSolved)
        Right(s"Unresolved constraints ${sol.unsolved}, type $t")
      else
        Left(t)
    }
  }

  def typecheckRec(e: Node_[Result]): Unit = {
    val res@(t, reqs, cons) = typecheckStep(e)
    val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ._3)
    val cs = subcs addNewConstraints cons
    // TODO: apply partial solution to reqs
//    val reqs2 = cs.applyPartialSolutionIt[T, Iterable[T], T](reqs.values.flatMap(_.values), p => p)
    e.typ = (cs applyPartialSolution t, reqs, cs.propagate)
  }

  def typecheckStep(e: Node_[Result]): StepResult = e.kind match {
    // worksheets
    case Spreadsheet =>
      val sheets = e.lits.asInstanceOf[Seq[String]]
      val (ts, reqss, _) = e.kids.seq.map(_.typ).unzip3
      val (mcons, mreqs) = mergeReqMaps(reqss)

      // TODO cross-saturate cells from different worksheets

      (TSpreadsheet(sheets.zip(ts).toMap), mreqs, mcons)

    // worksheets
    case Worksheet =>
      val positions = e.lits.asInstanceOf[Seq[BatchSpec]]
      val (ts, reqss, _) = e.kids.seq.map(_.typ).unzip3

      val (mcons, mreqs) = mergeReqMaps(reqss)

      // TODO saturate cells of this worksheet

      var types = Map[Int, Map[String, Type]]()

      for (i <- 0 until positions.size) {
        val (irow, icol) = positions(i)
        val t = ts(i)
        val row = types.get(irow) match {
          case None => Map[String, Type]()
          case Some(row) => row
        }
        val newrow = row + (icol -> t)
        types += (irow -> newrow)
      }

      var restReqs = Map[Int, Map[String, Type]]()
      var satcons = Seq[EqConstraint]()

      for ((irow, reqRow) <- mreqs) {
        val row = types.getOrElse(irow, Map())
        if (row.isEmpty)
          restReqs += irow -> reqRow
        else {
          val rest = reqRow.filter { ct =>
            val (icol, reqT) = ct
            row.get(icol) match {
              case None => true
              case Some(t) =>
                satcons = satcons :+ EqConstraint(reqT, t)
                false
            }
          }
          if (rest.nonEmpty)
            restReqs += irow -> rest
        }
      }

      (TWorksheet(types), restReqs, mcons ++ satcons)

      // TODO: need dynamic dependency instrumentation (rather than static, AST-induced dependencies)

    case Batch =>
      var ts = Seq[Type]()
      var mcons = Seq[EqConstraint]()
      var mreqs: Reqs = Map()

      for (i <- 0 until e.kids.seq.size) {
        val (t, reqs, _) = e.kids(i)
        ts = ts :+ t
        val (mc, mr) = mergeReqMaps(mreqs, reqs)
        mreqs = mr
        mcons = mcons ++ mc
      }

      (TBatch(ts), mreqs, mcons)

    // cells
    case Empty =>
      (TEmpty, Map(), Seq())

    case CInt =>
      (TInt, Map(), Seq())

    case CString =>
      (TString, Map(), Seq())

    case CFormula =>
      val (t, reqs, _) = e.typ
      (t, reqs, Seq())

    // formulas
    case Ref =>
      val (t, reqs, _) = e.kids(0).typ
      (t, reqs, Seq())

    case Range =>
      // TODO: Need to require something for a range of celss
      ???

    case Sum =>
      val (t, reqs, _) = e.kids(0).typ
      var cons = Seq(EqConstraint(TInt, t))
      var reqss = Seq(reqs)

      for (i <- 1 until e.kids.seq.size) {
        val (ti, reqsi, _) = e.kids(i).typ
        cons = cons :+ EqConstraint(t, ti)
        reqss = reqss :+ reqsi
      }

      val (mcons, mreqs) = mergeReqMaps(reqss)

      (TInt, mreqs, cons ++ mcons)

    case Counta =>
      var reqss = e.kids.seq.map(_.typ._2)
      val (mcons, mreqs) = mergeReqMaps(reqss)
      (TInt, mreqs, mcons)

    // cell references
    case CellRef =>
      val row = e.lits(0).asInstanceOf[Int]
      val col = e.lits(1).asInstanceOf[String]
      val U = freshUVar()
      (U, Map(row -> Map(col -> U)), Seq())

    case SheetCellRef =>
      val sheet = e.lits(0).asInstanceOf[String]
      val row = e.lits(1).asInstanceOf[Int]
      val col = e.lits(2).asInstanceOf[String]
      val U = freshUVar()
      // TODO: (U, Map(sheet -> Map(row -> Map(col -> U)), Seq()))
      ???

  }



  private val init: (Seq[Constraint], Reqs) = (Seq(), Map())

  def mergeReqMaps(req: Reqs, reqs: Reqs*): (Seq[Constraint], Reqs) = mergeReqMaps(req +: reqs)

  def mergeReqMaps(reqs: Seq[Reqs]): (Seq[Constraint], Reqs) =
    Util.timed(localState -> Statistics.mergeReqsTime) {
      reqs.foldLeft[(Seq[Constraint], Reqs)](init)(_mergeReqMaps)
    }

  private def _mergeReqMaps(was: (Seq[Constraint], Reqs), newReqs: Reqs) = {
    val wasReqs = was._2
    var mcons = was._1
    var mreqs = wasReqs
    for ((x, r2) <- newReqs)
      wasReqs.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) => {
          var rowchange = false
          var newrow = r1
          for ((y, t2) <- r2)
            r1.get(y) match {
              case None => {
                newrow += y -> t2
                rowchange = true
              }
              case Some(t1) =>
                mcons = mcons :+ EqConstraint(t1, t2)
            }
          if (rowchange)
            mreqs += x -> newrow
        }
      }
    (mcons, mreqs)
  }
}



case class BUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new BUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}