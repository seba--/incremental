package incremental.spreadsheet

import incremental.Node._
import incremental.{NodeKind, SyntaxChecking}
import constraints.equality._

/**
 * Created by seba on 13/11/14.
 */

abstract class IWorksheet(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
abstract class IRow(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
abstract class ICell(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
abstract class IFormula(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
abstract class ICellRef(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)

object IFormula {
  val cWorksheet = classOf[IWorksheet]
  val cRow = classOf[IRow]
  val cCell = classOf[ICell]
  val cFormula = classOf[IFormula]
  val cCellRef = classOf[ICellRef]
}
import IFormula._


// spreadsheet = SheetName -> Worksheet
case object Spreadsheet extends NodeKind(many(classOf[String], cWorksheet))

// worksheet = RowNum -> Row
case object Worksheet extends IWorksheet(many(classOf[Int], cRow))

// rows = ColName -> Cell
case object Row extends IRow(many(classOf[String], cCell))

// cells
case object Empty extends ICell(simple())
case object CInt extends ICell(simple(Seq(classOf[Int])))
case object CString extends ICell(simple(Seq(classOf[String])))
case object CFormula extends ICell(simple(cFormula))

// formulas
case object Ref extends IFormula(simple(cCellRef))
case object Range extends IFormula(simple(cCellRef, cCellRef))
case object Sum extends IFormula(many(cFormula))
case object Counta extends IFormula(many(cFormula))

// cell references
case object CellRef extends ICellRef(simple(Seq(classOf[String], classOf[Int])))
case object SheetCellRef extends ICellRef(simple(Seq(classOf[String], classOf[String], classOf[Int])))
