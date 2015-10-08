package incremental.spreadsheet

import incremental.Node._
import incremental.{NodeKind, SyntaxChecking}
import constraints.equality._

/**
 * Created by seba on 13/11/14.
 */

abstract class IWorksheet(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
abstract class IBatch(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
abstract class ICell(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
abstract class IFormula(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
abstract class ICellRef(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)

object IFormula {
  val cWorksheet = classOf[IWorksheet]
  val cBatch = classOf[IBatch]
  val cCell = classOf[ICell]
  val cFormula = classOf[IFormula]
  val cCellRef = classOf[ICellRef]
}
import IFormula._


// spreadsheet = SheetName -> Worksheet
case object Spreadsheet extends NodeKind(many(classOf[String], cWorksheet))

// worksheets
case object Worksheet extends IWorksheet(many(classOf[BatchSpec], cBatch))

sealed trait BatchSpec
case class RowBatch(row: Int, startCol: String, endCol: String)
case class ColBatch(col: String, startRow: Int, endRow: Int)

// batches
case object Batch extends IBatch(many(cCell))

// cells
case object Empty extends ICell(simple())
case object CInt extends ICell(simple(Seq(classOf[Int])))
case object CString extends ICell(simple(Seq(classOf[String])))
case object CFormula extends ICell(simple(cFormula))

// formulas
case object Ref extends IFormula(simple(cCellRef))
case object Range extends IFormula(simple(cCellRef, cCellRef))
case object Sum extends IFormula(many1(cFormula))
case object Counta extends IFormula(many1(cFormula))

// cell references
case object CellRef extends ICellRef(simple(Seq(classOf[Int], classOf[String])))
case object SheetCellRef extends ICellRef(simple(Seq(classOf[String], classOf[Int], classOf[String])))
