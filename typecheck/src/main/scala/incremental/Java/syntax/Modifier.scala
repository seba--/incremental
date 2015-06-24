package incremental.Java.syntax

/**
 * Created by qwert on 20.05.15.
 */

// Modifiers

trait Modifier
case class Public() extends Modifier with AbstractMethodMod with ConstantMod with InterfaceMod with FieldMod with MethodMod with ConstrMod with ClassMod
case class Private() extends Modifier with InterfaceMod with FieldMod with MethodMod with ConstrMod with ClassMod
case class Protected() extends Modifier with InterfaceMod with FieldMod with MethodMod with ConstrMod with ClassMod
case class Abstract() extends Modifier with AbstractMethodMod with InterfaceMod with MethodMod with ClassMod
case class Final() extends Modifier with ConstantMod with FieldMod with VarMod with MethodMod with ClassMod
case class Static() extends Modifier with ConstantMod with InterfaceMod with FieldMod with MethodMod with ClassMod
case class Native() extends Modifier with MethodMod
case class Transient() extends Modifier with FieldMod
case class Volatile() extends Modifier with FieldMod
case class SynchronizedMod() extends Modifier with MethodMod // name conflict with synchronized statement
case class StrictFP() extends Modifier with InterfaceMod with MethodMod with ClassMod

trait AbstractMethodMod
trait ConstantMod
trait InterfaceMod
trait FieldMod
trait VarMod // Final
trait MethodMod // Public, Protected, Private, Abstract, Static, Final, Synchronized, Native, StrictFP
trait ClassMod // Abstract, Public, Protected, Private, Static, Final, StrictFP
trait ConstrMod // Public, Protected, Private