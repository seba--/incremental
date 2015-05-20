package incremental.Java.syntax

/**
 * Created by qwert on 20.05.15.
 */

// Modifiers

trait Modifier
case class Public() extends Modifier
case class Private() extends Modifier
case class Protected() extends Modifier
case class Abstract() extends Modifier
case class Final() extends Modifier
case class Static() extends Modifier
case class Native() extends Modifier
case class Transient() extends Modifier
case class Volatile() extends Modifier
case class Synchronized() extends Modifier
case class StrictFP() extends Modifier