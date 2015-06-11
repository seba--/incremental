package incremental.Java.syntax

/**
 * Created by qwert on 20.05.15.
 */

// Modifiers

trait Modifier
case class Public() extends Modifier with AbstractMethodMod with ConstantMod with InterfaceMod
case class Private() extends Modifier with InterfaceMod
case class Protected() extends Modifier with InterfaceMod
case class Abstract() extends Modifier with AbstractMethodMod with InterfaceMod
case class Final() extends Modifier with ConstantMod
case class Static() extends Modifier with ConstantMod with InterfaceMod
case class Native() extends Modifier
case class Transient() extends Modifier
case class Volatile() extends Modifier
case class Synchronized() extends Modifier
case class StrictFP() extends Modifier with InterfaceMod

trait AbstractMethodMod
trait ConstantMod
trait InterfaceMod