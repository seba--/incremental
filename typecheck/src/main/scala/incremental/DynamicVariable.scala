package incremental

/**
 * Created by oliver on 09.06.15.
 *
 * Taken from the Scala standard library. We need a serializable version
 * for ScalaMeter.
 */
@SerialVersionUID(738095452354L)
class DynamicVariable[T](init: T) extends Serializable {
  private var tl = init

  /** Retrieve the current value */
  def value: T = tl

  /** Set the value of the variable while executing the specified
    * thunk.
    *
    * @param newval The value to which to set the variable
    * @param thunk The code to evaluate under the new setting
    */
  def withValue[S](newval: T)(thunk: => S): S = {
    val oldval = value
    tl = newval

    try thunk
    finally tl = oldval
  }

  /** Change the currently bound value, discarding the old value.
    * Usually withValue() gives better semantics.
    */
  def value_=(newval: T) = tl = newval

  override def toString: String = "DynamicVariable(" + value + ")"
}
