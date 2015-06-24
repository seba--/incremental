package incremental.Java.syntax

/**
 * Created by qwert on 20.05.15.
 */

// Modifiers

trait Modifier
case class Public() extends Modifier with AbstractMethodMod with ConstantMod with InterfaceMod with FieldMod with MethodMod with ConstrMod
case class Private() extends Modifier with InterfaceMod with FieldMod with MethodMod with ConstrMod
case class Protected() extends Modifier with InterfaceMod with FieldMod with MethodMod with ConstrMod
case class Abstract() extends Modifier with AbstractMethodMod with InterfaceMod with MethodMod
case class Final() extends Modifier with ConstantMod with FieldMod with VarMod with MethodMod
case class Static() extends Modifier with ConstantMod with InterfaceMod with FieldMod with MethodMod
case class Native() extends Modifier with MethodMod
case class Transient() extends Modifier with FieldMod
case class Volatile() extends Modifier with FieldMod
case class Synchronized() extends Modifier with MethodMod
case class StrictFP() extends Modifier with InterfaceMod with MethodMod

trait AbstractMethodMod
trait ConstantMod
trait InterfaceMod
trait FieldMod
trait VarMod // Final
trait MethodMod // Public, Protected, Private, Abstract, Static, Final, Synchronized, Native, StrictFP
trait ClassMod
trait ConstrMod // Public, Protected, Private