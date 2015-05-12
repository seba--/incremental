package incremental.pcf.with_references

import incremental.Node._
import incremental.NodeKind
import incremental.pcf.Exp_$
import incremental.pcf.Exp_._

/**
 * Created by seba on 15/11/14.
 */
case object Ref extends Exp_(simple(cExp))
case object Deref extends Exp_(simple(cExp))
case object Assign extends Exp_(simple(cExp, cExp))
case object Seq extends Exp_(simple(cExp, cExp))
