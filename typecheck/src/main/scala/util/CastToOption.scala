package util

/**
  * Created by oliver on 24.04.17.
  */
object CastToOption {
  /**
    * Wraps casts into Option. Enables more idiomatic for-comprehensions when
    * nesting casts.
    *
    * @param x
    * @tparam T
    */
  implicit class CastOps[T](val x: T) extends AnyVal {
    def as[V: Manifest]: Option[V] = if (manifest.runtimeClass.isInstance(x)) Some(x.asInstanceOf[V]) else None
  }
}
