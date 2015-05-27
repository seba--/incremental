package incremental.Java.syntax

/**
 * Created by qwert on 27.05.15.
 */

// Package Declaration
case class PackageDec(annotation: Seq[Anno], name: PackageName)
// Anno* "package" PackageName ";" -> PackageDec {cons("PackageDec")}

// Import Declarations
trait ImportDec
case class TypeImportDec(t: TypeName) extends ImportDec
case class TypeImportOnDemandDec(p: PackageName) extends ImportDec
case class StaticImportDec(t: TypeName, id: String) extends ImportDec
case class StaticImportOnDemandDec(t: TypeName) extends ImportDec
