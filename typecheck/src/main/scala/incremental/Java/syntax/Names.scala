package incremental.Java.syntax

/**
 * Created by qwert on 04.04.15.
 */
trait Name {

}

// {Id "."}+ -> PackageName {cons("PackageName")}
case class PackageName(path: Seq[String]) extends Name

trait AmbName extends Name{}
case class AmbNameT(id: String) extends AmbName
case class AmbNameExt(ext: AmbName, id: String) extends AmbName

trait TypeName extends Name with TypeDecSpec with ArrayBaseType{}
case class TypeNameT(id: String) extends TypeName
case class TypeNameExt(ext: PackageOrTypeName, id: String) extends TypeName

trait ExprName extends Name with LHS{}
case class ExprNameT(id: String) extends ExprName
case class ExprNameExt(ext: AmbName, id: String) extends ExprName

trait MethodName extends Name{}
case class MethodNameT(id: String) extends MethodName
case class MethodNameExt(ext: AmbName, id: String) extends MethodName

trait PackageOrTypeName extends Name{}
case class PackageOrTypeNameT(id: String) extends PackageOrTypeName
case class PackageOrTypeNameExt(ext: PackageOrTypeName, id: String) extends PackageOrTypeName