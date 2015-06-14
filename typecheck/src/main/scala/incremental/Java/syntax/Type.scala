package incremental.Java.syntax

/**
 * Created by qwert on 04.04.15.
 */
trait Type{

}

// Primitive Types
trait PrimType extends Type with ArrayBaseType{}

case class TBoolean() extends PrimType

trait NumType extends PrimType{}
trait IntType extends NumType{}
trait FloatType extends NumType{}

case class TByte() extends IntType
case class TShort() extends IntType
case class TInt() extends IntType
case class TLong() extends IntType
case class TChar() extends IntType
case class TFloat() extends FloatType
case class TDouble() extends FloatType

// Reference Types
trait RefType extends Type{}

case class ClassOrInterfaceType(typename: TypeDecSpec, typeArgs: Option[TypeArgs]) extends RefType
case class ClassType(typename: TypeDecSpec, typeArgs: Option[TypeArgs]) extends RefType with ExceptionType
case class InterfaceType(typename: TypeDecSpec, typeArgs: Option[TypeArgs]) extends RefType

trait TypeDecSpec //TODO: extends TypeName{}
// TypeName -> TypeDecSpec
case class Member(typeDecSpec: TypeDecSpec, typeArgs: TypeArgs, id: String) extends TypeDecSpec

/* TODO: ref type or type or nothing or ...? is it even a case class?
 * TypeVarId -> TypeVar {cons("TypeVar")}
 * ClassOrInterfaceType -> RefType
 * ArrayType            -> RefType
 * but declared in module for reference types
 *
 * PrimType -> Type
 * RefType  -> Type
 */
case class TypeVar(id: TypeVarId) extends Type


case class ArrayType(t: Type) extends RefType

// Type Variables
// TODO: case classes?
case class TypeParam(id: TypeVarId, typeBound: Option[TypeBound])
case class TypeBound(extendsTypes: Seq[ClassOrInterfaceType])
case class TypeParams(typeParams: Seq[TypeParam])
case class TypeVarId(id: String) // Id -> TypeVarId TODO: only production, inline?

// Parametrized Types
case class TypeArgs(args: Seq[ActualTypeArg])

trait ActualTypeArg extends Type{}
case class Wildcard(wildcard: WildcardBound) extends ActualTypeArg

trait WildcardBound{}
case class WildcardUpperBound(extendsT: RefType) extends WildcardBound
case class WildcardLowerBound(superT: RefType) extends WildcardBound

trait ResultType extends Type
case class Void() extends ResultType