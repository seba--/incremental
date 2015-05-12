package incremental

trait Node_[T] {
  def withType[T] = this.asInstanceOf[Node_[T]]

  private var _typ: T = _
  private var _valid = false
  def valid = _valid // needed for propagation pruning
  def typ = _typ
  def typ_=(t: T): Unit = {
    _typ = t
    _valid = true
  }
  def invalidate: Unit = {
    _typ = null.asInstanceOf[T]
    _valid = false
    foreachKid(n => n.invalidate)
  }

  def changed(): Unit = {
    _valid = false
    // TODO update size and height
  }

  def foldKids[R](init: R)(f: (R, Node_[T]) => R): R
  def updateKid(i: Int, n: Node_[T]): Unit

  def foreachKid[R](f: Node_[T] => Unit): Unit = foldKids[Unit](Unit)((s, n) => f(n))
  def mapKids[R](f: Node_[T] => R): Seq[R] = foldKids(Seq[R]())((s, n) => s :+ f(n))
  def kidSeq = foldKids(Seq[Node_[T]]())((s, n) => s :+ n)
  def kidCount: Int = foldKids(0)((i,k) => i+1)
  def hasKids: Boolean = foldKids(false)((b,k) => return true)

  object kids {
    def apply(i: Int): Node_[T] = {
      foldKids(i)((i, k) => if (i == 0) return k else i - 1)
      throw new IndexOutOfBoundsException
    }
    def update(i: Int, n: Node_[T]) = {
      if (n._valid)
        _valid = false
      updateKid(i, n)
      _height = maxHeight
      _size = sumSize
    }
  }

  private var _height: Int = maxHeight
  def height = _height
  protected def maxHeight: Int = {
    val incr = if (hasKids) 1 else 0
    foldKids(0)((i, n) => i.max(n._height)) + incr
  }

  private var _size = sumSize
  def size = _size
  protected def sumSize: Int = foldKids(1)((s, n) => s + n._size)


  def uninitialized: Seq[Node_[T]] = {
    val buf = collection.mutable.ArrayBuffer[Node_[T]]()
    uninitialized(buf)
    buf
  }

  def uninitialized(buf: collection.mutable.ArrayBuffer[Node_[T]]): Unit = {
    val oldsize = buf.size
    foreachKid(_.uninitialized(buf))
    val hasSubchange = oldsize == buf.size
    if (!valid || hasSubchange)
      buf += this
  }

  def visitUninitialized(f: Node_[T] => Boolean): Boolean = {
    val hasSubchange = foldKids(false)((changed, k) =>  k.visitUninitialized(f) || changed)
    if (!valid || hasSubchange)
      f(this)
    else
      false
  }

  def visitUninitialized2(f: Node_[T] => (T, Boolean)): Boolean = {
    val hasSubchange = foldKids(false)((changed, k) =>  k.visitUninitialized2(f) || changed)
    if (!valid || hasSubchange) {
      val (t, doContinue) = f(this)
      _typ = t
      _valid = true
      doContinue
    }
    else
      false
  }

  def visitInvalid(f: Node_[T] => Boolean): Boolean = {
    val hasSubchange = foldKids(false)((changed, k) => if (!k.valid) changed || k.visitInvalid(f) else changed)

    if (!valid || hasSubchange)
      f(this)
    else
      false
  }

  def visitUpto(depth: Int)(f: Node_[T] => Boolean): Boolean = {
    if (depth > 0) {
      val hasSubchange = foldKids(false){ (changed, k) => k.visitUpto(depth - 1)(f) || changed }
      if (!valid || hasSubchange)
        f(this)
      else false
    }
    else
      false
  }
}

object Node {
  type Node = Node_[Any]
}
