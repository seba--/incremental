package incremental.java.syntax

import incremental.Node._
import incremental.java.syntax.expr.{ArrayBaseType, Expr}

/**
 * Created by qwert on 04.04.15.
 */
// {Id "."}+ -> PackageName {cons("PackageName")}
case class PackageName(path: Seq[String])

trait AmbName
case class AmbNameT(id: String) extends AmbName
case class AmbNameExt(ext: AmbName, id: String) extends AmbName

trait TypeName extends TypeDecSpec with ArrayBaseType{}
case class TypeNameT(id: String) extends TypeName
case class TypeNameExt(ext: PackageOrTypeName, id: String) extends TypeName

//trait ExprName
//case class ExprNameT(id: String) extends ExprName
//case class ExprNameExt(ext: AmbName, id: String) extends ExprName

trait MethodName
case class MethodNameT(id: String) extends MethodName
case class MethodNameExt(ext: AmbName, id: String) extends MethodName

trait PackageOrTypeName
case class PackageOrTypeNameT(id: String) extends PackageOrTypeName
case class PackageOrTypeNameExt(ext: PackageOrTypeName, id: String) extends PackageOrTypeName

trait NT_ExprName
case object ExprName extends Expr(simple(Seq(classOf[String])) orElse simple(Seq(classOf[AmbName], classOf[String]))) with NT_ExprName

trait Id extends NT_VarDecId
case class ID(s: String) extends Id