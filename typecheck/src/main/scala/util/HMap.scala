package util

import scala.collection.GenTraversableOnce
import scala.collection.generic.Subtractable
import scala.language.higherKinds

object HMap {

  trait Key[+T]

  type NHMap[V <: AnyRef, K[+_] <: Key[_]] = Map[K[V], V]

  implicit class HMapOps[V <: AnyRef, K[+_] <: Key[_]](m: NHMap[V, K]) {
    def hget[V2 <: V](k: K[V2]): Option[V2] = m.get(k).asInstanceOf[Option[V2]]
    def hgetOrElse[V2 <: V](k: K[V2], default: V2): V2 = hget(k).getOrElse(default)
    def --(ks: GenTraversableOnce[K[_]]) = m -- ks.asInstanceOf[GenTraversableOnce[K[V]]]
//    def +[V2 <: V, Key <: K[V2]](kv: (Key, V2)): NHMap[V, K] = m + kv
//    def hmapValues[V2 <: AnyRef](f: V => V): NHMap[V, K] = m mapValues f
  }

  def empty[V <: AnyRef, K[+_] <: Key[_]]: NHMap[V, K] = Map()
}


//import HMap._
//
//trait HMap[V <: AnyRef, K[_] <: Key[_]]
//  extends Iterable[(K[V], V)]
//     with Subtractable[K[_], HMap[V, K]]
//     {
//
//  def hmapget[V2 <: V](k: K[V2]): Option[V2]
//  def hmapgetOrElse[V2 <: V](k: K[V2], default: => V2): V2 = hmapget(k).getOrElse(default)
//
//  def @@+[V2 <: V, Key <: K[V2]](kv: (Key, V2)): HMap[V, K]
//  def @@++[V2 <: V](m2: HMap[V2, K]): HMap[V, K]
//
//  def hmapmap[V2 <: AnyRef](f: (K[V],V) => (K[V2], V2)): HMap[V2, K]
//  def hmapmapValues[V2 <: AnyRef](f: V => V2): HMap[V2, K]
//}
//
//private class HMapImpl[V <: AnyRef, K[+_] <: Key[_]](m: Map[K[V], AnyRef]) extends HMap[V, K] {
//  def hmapget[V2 <: V](k: K[V2]) = m.get(k).asInstanceOf[Option[V2]]
//  def @@+[V2 <: V, Key <: K[V2]](kv: (Key, V2)) = new HMapImpl[V, K](m + kv)
//}
//
