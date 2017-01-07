package util

import scala.collection.{Seq, mutable}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ListBuffer

sealed trait TreeSeq[T] extends Iterable[T] with Serializable {

  def ++(t: TreeSeq[T]): TreeSeq[T] = new Node(this, t)

  def ++(ts: Traversable[T]): TreeSeq[T] = new Node(this, TreeSeq(ts.toSeq))

  def +:(t: T): TreeSeq[T]

  def size: Int

  override def toString(): String = iterator.toSeq.toString
}

class Empty[T] extends TreeSeq[T] {
  override def +:(t: T) = new Leave(Seq(t))

  override def size: Int = 0

  override def iterator: Iterator[T] = Iterator.empty
}

class Leave[T](val ts: Seq[T]) extends TreeSeq[T] {
  override def +:(t: T) = new Leave[T](t +: ts)

  override def iterator: Iterator[T] = ts.iterator

  override def size: Int = ts.length
}

class Node[T](l: TreeSeq[T], r: TreeSeq[T]) extends TreeSeq[T] {
  override def +:(t: T) = new Node(new Leave(Seq(t)), this)

  override def iterator: Iterator[T] = l.iterator ++ r.iterator

  override def size: Int = l.size + r.size
}



object TreeSeq {
  def apply[T](): TreeSeq[T] = new Empty[T]
  def apply[T](t: T, ts: T*): TreeSeq[T] = new Leave[T](t +: ts)
  def apply[T](ts: Seq[T]): TreeSeq[T] = new Leave(ts)

  implicit def canBuildFrom[A]: CanBuildFrom[Iterable[A], A, TreeSeq[A]] = new CanBuildFrom[Iterable[A], A, TreeSeq[A]] {
    override def apply(from: Iterable[A]): mutable.Builder[A, TreeSeq[A]] = new TreeSeqBuilder[A] ++= from
    override def apply(): mutable.Builder[A, TreeSeq[A]] = new TreeSeqBuilder[A]
  }
}

class TreeSeqBuilder[A] extends mutable.Builder[A, TreeSeq[A]] {
  private val list: ListBuffer[A] = ListBuffer.empty

  override def +=(elem: A): TreeSeqBuilder.this.type = {list += elem; this}

  override def clear(): Unit = list.clear()

  override def result(): TreeSeq[A] = new Leave(list)
}

