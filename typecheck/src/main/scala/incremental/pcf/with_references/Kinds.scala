package incremental.pcf.with_references

import incremental.Node._
import incremental.NodeKind

/**
 * Created by seba on 15/11/14.
 */
case object Ref extends NodeKind(simple(1))
case object Deref extends NodeKind(simple(1))
case object Assign extends NodeKind(simple(2))
case object Seq extends NodeKind(simple(2))
