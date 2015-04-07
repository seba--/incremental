package incremental.pcf.with_references

import incremental.Exp._
import incremental.{SimpleSyntax, ExpKind}

/**
 * Created by seba on 15/11/14.
 */
case object Ref extends ExpKind(simple(1))
case object Deref extends ExpKind(simple(1))
case object Assign extends ExpKind(simple(2))
case object Seq extends ExpKind(simple(2))
