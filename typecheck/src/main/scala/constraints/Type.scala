package constraints

trait Type

trait TypeCompanion extends Serializable {
  type TError
  type TSubst
}
object TypCompanion {
  implicit def companion[T <: Type](implicit comp: TypeCompanion): TypeCompanion = comp
}
