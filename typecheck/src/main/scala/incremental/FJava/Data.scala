package incremental

import incremental.Exp.Exp
import incremental.Type.{Argument, Name}
import sun.security.krb5.internal.crypto.EType

/**
 * Created by lirakuci on 3/2/15.
 */
case object Data


case class Class(val name : Name, superClass : Name, val fields : List[Field], val m : List[Method] )

case class Field(val ftype : Type, fname : Name)

case class Method(val mtype : Type, mname : Name,margs : List[Argument])


trait Expr extends Exp {
  }
case class ExpVar(val ename : Name) extends Expr

case class ExpField(val ename : Name, val eexpr : Exp) extends Expr

case class ExpMethod(val ename : Name, eexpr : Exp, eexprs : List[Exp]) extends Expr

case class ExpNew(val eType: Type, eexprs : List[Exp] ) extends Expr

case class  ExpCast(etype : Type, eexpr : Expr) extends Expr

