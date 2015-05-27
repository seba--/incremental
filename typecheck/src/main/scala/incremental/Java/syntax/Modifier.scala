package incremental.Java.syntax

/**
 * Created by qwert on 20.05.15.
 */

// Modifiers

trait Modifier
case class Public() extends Modifier with AbstractMethodMod with ConstantMod
case class Private() extends Modifier
case class Protected() extends Modifier
case class Abstract() extends Modifier with AbstractMethodMod
case class Final() extends Modifier with ConstantMod
case class Static() extends Modifier with ConstantMod
case class Native() extends Modifier
case class Transient() extends Modifier
case class Volatile() extends Modifier
case class Synchronized() extends Modifier
case class StrictFP() extends Modifier

trait AbstractMethodMod
trait ConstantMod