package incremental.concurrent

import akka.actor.{ActorRef, ActorSystem, Actor, Props}
import incremental.Node.Lit
import incremental.{NodeKind, Node_}

import scala.reflect.ClassTag


sealed trait Message
case object Start extends Message
case class Dirty(index: Int) extends Message
case class Update[T](index: Int, value: Node_[T]) extends Message
case class Done(index: Int, result: Any) extends Message //TODO try to come up with parameterized solution

/**
 * Created by oliver on 24.04.15.
 */
class NodeActor[T: ClassTag](protected val node: Node_[T], protected val index: Int, f: Node_[T] => Boolean, trigger: ActorRef) extends Actor {
  protected val sink = context.parent
  private var _counter = node.kids.seq.length

  def receive = {
    case Done(i, res) =>
      val result: T = res.asInstanceOf[T]
      node.kids(i).typ = result
      _counter = _counter - 1
     tryApply()
  }

  def tryApply() = {
    if (_counter == 0) {
      f(node)
      sink ! Done(index, node.typ)
      //context.stop(self)
    }
  }

  override def preStart() = {
    if (node.kids.seq.isEmpty) {
      context.become(leaf)
      trigger ! Register
    }

    for((i, k) <- (0 until node.kids.seq.length) zip node.kids.seq)
      context.actorOf(Props(new NodeActor[T](k, i, f, trigger)))
  }

  val leaf: Receive = {
    case Start =>
      tryApply()
  }

}


sealed trait TriggerMsg
case object Register extends TriggerMsg

class Trigger extends Actor {
  private var leafs: List[ActorRef] = Nil
  private var active: Boolean = false

  def receive = {
    case Register =>
      leafs = sender() :: leafs
      if (active)
        sender() ! Start

    case Start =>
      if (!active) {
        for (leaf <- leafs)
          leaf ! Start
        active = true
      }
  }
}

class RootNodeActor[T: ClassTag](receiver: ActorRef, node: Node_[T], index: Int, f: Node_[T] => Boolean, trigger: ActorRef) extends NodeActor[T](node, index, f, trigger) {
  override protected val sink = receiver
}