package util

import scala.collection.GenTraversableOnce
import scala.language.higherKinds

object HMap {

  trait Key[+T]

  type NHMap[V <: AnyRef, K[+_] <: Key[_]] = Map[K[V], V]

  implicit class HMapOps[V <: AnyRef, K[+_] <: Key[_]](val m: NHMap[V, K]) extends AnyVal {
    def hget[V2 <: V](k: K[V2]): Option[V2] = m.get(k).asInstanceOf[Option[V2]]
    def hgetOrElse[V2 <: V](k: K[V2], default: V2): V2 = hget(k).getOrElse(default)
    def --(ks: GenTraversableOnce[K[_]]) = m -- ks.asInstanceOf[GenTraversableOnce[K[V]]]
  }

  def empty[V <: AnyRef, K[+_] <: Key[_]]: NHMap[V, K] = Map()
}
