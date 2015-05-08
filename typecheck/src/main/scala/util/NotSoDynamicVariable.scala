package util



/**
 * Created by oliver on 07.05.15.
 */
class NotSoDynamicVariable[T](init :T) {

  private var _value: T = init

  /** Retrieve the current value */
  def value: T = _value

  /** Set the value of the variable while executing the specified
    * thunk.
    *
    * @param newval The value to which to set the variable
    * @param thunk The code to evaluate under the new setting
    */
  def withValue[S](newval: T)(thunk: => S): S = {
    val oldval = value
    _value = newval

    try thunk
    finally _value = oldval
  }

  /** Change the currently bound value, discarding the old value.
    * Usually withValue() gives better semantics.
    */
  def value_=(newval: T) = _value = newval

  override def toString: String = "NotSoDynamicVariable(" + value + ")"
}
