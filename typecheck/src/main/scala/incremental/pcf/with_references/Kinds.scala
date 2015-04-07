package incremental.pcf.with_references

import incremental.{Syntax, ExpKind}

/**
 * Created by seba on 15/11/14.
 */
case object Ref extends ExpKind(Syntax(1))
case object Deref extends ExpKind(Syntax(1))
case object Assign extends ExpKind(Syntax(2))
case object Seq extends ExpKind(Syntax(2))
