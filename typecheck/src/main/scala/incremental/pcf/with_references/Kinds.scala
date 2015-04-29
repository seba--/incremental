package incremental.pcf.with_references

import incremental.Node._
import incremental.NodeKind
import incremental.pcf.Exp
import incremental.pcf.Exp._

/**
 * Created by seba on 15/11/14.
 */
case object Ref extends Exp(simple(cExp))
case object Deref extends Exp(simple(cExp))
case object Assign extends Exp(simple(cExp, cExp))
case object Seq extends Exp(simple(cExp, cExp))
