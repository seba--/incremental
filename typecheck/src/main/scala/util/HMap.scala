package util

import scala.collection.generic.Subtractable
import scala.language.higherKinds

object HMap {
  trait Key[+T]

  def empty[V <: AnyRef, K[_] <: Key[_]]: HMap[V, K] = new HMapImpl[V, K](Map())
}
import HMap._

trait HMap[V <: AnyRef, K[_] <: Key[_]]
  extends Iterable[(K[V], V)]
     with Subtractable[K[_], HMap[V, K]]
     {

  def get[V2 <: V](k: K[V2]): Option[V2]
  def getOrElse[V2 <: V](k: K[V2], default: => V2): V2 = get(k).getOrElse(default)

  def +[V2 <: V, Key <: K[V2]](kv: (Key, V2)): HMap[V, K]
  def ++[V2 <: V](m2: HMap[V2, K]): HMap[V, K]

  def map[V2 <: AnyRef](f: (K[V],V) => (K[V2], V2)): HMap[V2, K]
  def mapValues[V2 <: AnyRef](f: V => V2): HMap[V2, K]
}

private class HMapImpl[V <: AnyRef, K[+_] <: Key[_]](m: Map[K[V], AnyRef]) extends HMap[V, K] {
  def get[V2 <: V](k: K[V2]) = m.get(k).asInstanceOf[Option[V2]]
  def +[V2 <: V, Key <: K[V2]](kv: (Key, V2)) = new HMapImpl[V, K](m + kv)
}

