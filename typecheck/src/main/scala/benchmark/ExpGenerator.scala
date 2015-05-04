package benchmark

import incremental.Node._
import incremental.NodeKind
import constraints.Type

/**
 * Created by seba on 05/11/14.
 */
object ExpGenerator {
  trait LeaveMaker {
    def reset()
    def next(): Node
  }
  def constantLeaveMaker(c: => Node): LeaveMaker = new LeaveMaker {
    override def next(): Node = c
    override def reset(): Unit = {}
  }
  def stateLeaveMaker[T](stateInit: T, stateUpdate: T => T, treeMaker: T => Node): LeaveMaker = {
    object maker extends LeaveMaker {
      var state: T = stateInit
      override def next(): Node = {
        val t = treeMaker(state)
        state = stateUpdate(state)
        t
      }
      override def reset(): Unit = state = stateInit
    }
    maker
  }

  def makeBinTree(height: Int, kind: NodeKind, leaveMaker: LeaveMaker, sharing: Boolean = false): Node = {
    val leaveCount = Math.pow(2, height-1).toInt
    val ts = Array.ofDim[Node](leaveCount)
    leaveMaker.reset()

    for (i <- 0 until leaveCount)
      ts(i) = leaveMaker.next()

    for (h <- height to 1 by -1)
      for (i <- 0 until Math.pow(2, h-1).toInt-1 by 2) {
        val l = ts(i)
        val r = ts(i+1)
        if (sharing && l == r)
          ts(i/2) = kind(l, l)
        else
          ts(i/2) = kind(l, r)
      }
    ts(0)
  }

  def usedVars(h: Int) = for (j <- (1 to Math.pow(2, h-1).toInt).toSeq) yield Symbol(s"x$j")

  def makeFunType[T <: Type](height: Int, returnType: T, argMaker: () => T, tfun: (T,T)=>T): T = {
    val length = Math.pow(2,height-1).toInt
    var argTypes = Seq[T]()
    for (i <- 1 to length)
      argTypes = argMaker() +: argTypes
    var t = returnType
    for (i <- 1 to length) {
      t = tfun(argTypes.head, t)
      argTypes = argTypes.tail
    }
    t
  }

  def makeBinType(height: Int, returnType: Type, argMaker: () => Type, tconstr: (Type, Type) => Type): Type = {
    val length = Math.pow(2,height-1).toInt
    var argTypes = Seq[Type]()
    for (i <- 1 to length)
      argTypes = argMaker() +: argTypes
    var t = returnType
    for (i <- 1 to length) {
      t = tconstr(argTypes.head, t)
      argTypes = argTypes.tail
    }
    t
  }
}
