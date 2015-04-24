package constraints

trait Type



//
//object TypCompanion {
//  implicit def companion[T <: Typ[T]](implicit comp: TypCompanion[T]): TypCompanion[T] = comp
//}
////implicits trick for per-type class instance common definitions
//trait TypCompanion[T <: Typ[T]] extends Serializable {
//  type TError = String
//  type TSubst = Map[Symbol, T]
//  type UVar <: T
//}