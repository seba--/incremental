package experiment

import scala.language.implicitConversions

import scala.reflect.SourceContext
import scala.virtualization.lms.common.{TupleOps, EffectExp, Base}

trait MapOps extends Base with TupleOps {
  object Map {
    def apply[K:Manifest,V:Manifest](elems: (K, V)*)(implicit pos: SourceContext) = map_new[K,V](elems:_*)
  }

  implicit def repMapToMapOps[K:Manifest,V:Manifest](m: Rep[Map[K,V]]) = new mapOpsCls(m)

  class mapOpsCls[K:Manifest,V:Manifest](m: Rep[Map[K,V]]) {
    def apply(k: Rep[K])(implicit pos: SourceContext) = map_apply(m, k)
    def get(k: Rep[K])(implicit pos: SourceContext) = map_get(m, k)
    def updated(k: Rep[K], v: Rep[V])(implicit pos: SourceContext) = map_updated(m,k,v)
    def +(kv: Rep[(K,V)])(implicit pos: SourceContext) = map_updated(m,kv._1, kv._2)
    def contains(k: Rep[K])(implicit pos: SourceContext) = map_contains(m, k)
    def size(implicit pos: SourceContext) = map_size(m)
    def values(implicit pos: SourceContext) = map_values(m)
    def keySet(implicit pos: SourceContext) = map_keyset(m)
    def keys(implicit pos: SourceContext) = map_keys(m)
    def iterator(implicit pos: SourceContext) = map_iterator(m)
  }

  def map_new[K:Manifest,V:Manifest](elems: (K, V)*)(implicit pos: SourceContext) : Rep[Map[K,V]]
  def map_apply[K:Manifest,V:Manifest](m: Rep[Map[K,V]], k: Rep[K])(implicit pos: SourceContext): Rep[V]
  def map_get[K:Manifest,V:Manifest](m: Rep[Map[K,V]], k: Rep[K])(implicit pos: SourceContext): Rep[Option[V]]
  def map_updated[K:Manifest,V:Manifest](m: Rep[Map[K,V]], k: Rep[K], v: Rep[V])(implicit pos: SourceContext): Rep[Map[K, V]]
  def map_minus[K:Manifest,V:Manifest](m: Rep[Map[K,V]], k: Rep[K])(implicit pos: SourceContext): Rep[Map[K, V]]
  def map_contains[K:Manifest,V:Manifest](m: Rep[Map[K,V]], i: Rep[K])(implicit pos: SourceContext): Rep[Boolean]
  def map_size[K:Manifest,V:Manifest](m: Rep[Map[K,V]])(implicit pos: SourceContext): Rep[Int]
  def map_values[K:Manifest,V:Manifest](m: Rep[Map[K,V]])(implicit pos: SourceContext): Rep[Iterable[V]]
  def map_keyset[K:Manifest,V:Manifest](m: Rep[Map[K,V]])(implicit pos: SourceContext): Rep[Set[K]]
  def map_keys[K:Manifest,V:Manifest](m: Rep[Map[K,V]])(implicit pos: SourceContext): Rep[Iterable[K]]
  def map_iterator[K:Manifest,V:Manifest](m: Rep[Map[K,V]])(implicit pos: SourceContext): Rep[Iterable[(K,V)]]
}

trait MapOpsExp extends MapOps with EffectExp {

  abstract class MapDef[K:Manifest,V:Manifest,R:Manifest] extends Def[R] {
    val mK = manifest[K]
    val mV = manifest[V]
  }

  case class MapNew[K: Manifest, V: Manifest](elems: (K, V)*) extends MapDef[K, V, Map[K,V]]
  case class MapApply[K: Manifest, V: Manifest](m: Rep[Map[K, V]], k: Rep[K]) extends MapDef[K, V, V]
  case class MapGet[K: Manifest, V: Manifest](m: Rep[Map[K, V]], k: Rep[K]) extends MapDef[K, V, Option[V]]
  case class MapUpdated[K: Manifest, V: Manifest](m: Rep[Map[K, V]], k: Rep[K], v: Rep[V]) extends MapDef[K, V, Map[K,V]]
  case class MapMinus[K: Manifest, V: Manifest](m: Rep[Map[K, V]], k: Rep[K]) extends MapDef[K, V, Map[K,V]]
  case class MapContains[K: Manifest, V: Manifest](m: Rep[Map[K, V]], i: Rep[K]) extends MapDef[K, V, Boolean]
  case class MapSize[K: Manifest, V: Manifest](m: Rep[Map[K, V]]) extends MapDef[K, V, Int]
  case class MapValues[K: Manifest, V: Manifest](m: Rep[Map[K, V]]) extends MapDef[K, V, Iterable[V]]
  case class MapKeyset[K: Manifest, V: Manifest](m: Rep[Map[K, V]]) extends MapDef[K, V, Set[K]]
  case class MapKeys[K: Manifest, V: Manifest](m: Rep[Map[K, V]]) extends MapDef[K, V, Iterable[K]]
  case class MapIterator[K: Manifest, V: Manifest](m: Rep[Map[K, V]]) extends MapDef[K, V, Iterable[(K,V)]]


  override def map_new[K: Manifest, V: Manifest](elems: (K, V)*)(implicit pos: SourceContext) = MapNew(elems:_*)
  override def map_apply[K: Manifest, V: Manifest](m: Rep[Map[K, V]], k: Rep[K])(implicit pos: SourceContext) = MapApply(m, k)
  override def map_get[K:Manifest,V:Manifest](m: Rep[Map[K,V]], k: Rep[K])(implicit pos: SourceContext) = MapGet(m, k)
  override def map_updated[K: Manifest, V: Manifest](m: Rep[Map[K, V]], k: Rep[K], v: Rep[V])(implicit pos: SourceContext) = MapUpdated(m, k, v)
  override def map_minus[K: Manifest, V: Manifest](m: Rep[Map[K, V]], k: Rep[K])(implicit pos: SourceContext) = MapMinus(m, k)
  override def map_contains[K: Manifest, V: Manifest](m: Rep[Map[K, V]], i: Rep[K])(implicit pos: SourceContext) = MapContains(m, i)
  override def map_size[K: Manifest, V: Manifest](m: Rep[Map[K, V]])(implicit pos: SourceContext) = MapSize(m)
  override def map_values[K: Manifest, V: Manifest](m: Rep[Map[K, V]])(implicit pos: SourceContext) = MapValues(m)
  override def map_keyset[K: Manifest, V: Manifest](m: Rep[Map[K, V]])(implicit pos: SourceContext) = MapKeyset(m)
  override def map_keys[K: Manifest, V: Manifest](m: Rep[Map[K, V]])(implicit pos: SourceContext) = MapKeys(m)
  override def map_iterator[K: Manifest, V: Manifest](m: Rep[Map[K, V]])(implicit pos: SourceContext) = MapIterator(m)
}