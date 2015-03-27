package incremental.pcf.with_references

import incremental.ExpKind

/**
 * Created by seba on 15/11/14.
 */
case object Ref extends ExpKind
case object Deref extends ExpKind
case object Assign extends ExpKind
case object Seq extends ExpKind
