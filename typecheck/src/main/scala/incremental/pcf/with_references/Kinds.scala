package incremental.pcf.with_references

import incremental.ExpKind

/**
 * Created by seba on 15/11/14.
 */
case object Ref extends ExpKind(1)
case object Deref extends ExpKind(1)
case object Assign extends ExpKind(2)
case object Seq extends ExpKind(2)
