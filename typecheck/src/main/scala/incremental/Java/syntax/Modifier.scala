package incremental.Java.syntax

/**
 * Created by qwert on 20.05.15.
 */

// Modifiers

trait Modifier
case class Public() extends Modifier with AbstractMethodMod with ConstantMod with InterfaceMod with FieldMod
case class Private() extends Modifier with InterfaceMod with FieldMod
case class Protected() extends Modifier with InterfaceMod with FieldMod
case class Abstract() extends Modifier with AbstractMethodMod with InterfaceMod
case class Final() extends Modifier with ConstantMod with FieldMod
case class Static() extends Modifier with ConstantMod with InterfaceMod with FieldMod
case class Native() extends Modifier
case class Transient() extends Modifier with FieldMod
case class Volatile() extends Modifier with FieldMod
case class Synchronized() extends Modifier
case class StrictFP() extends Modifier with InterfaceMod

trait AbstractMethodMod
trait ConstantMod
trait InterfaceMod
trait FieldMod