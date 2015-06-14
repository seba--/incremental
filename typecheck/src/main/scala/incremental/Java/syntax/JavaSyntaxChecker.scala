package incremental.Java.syntax

import incremental.Node._
import incremental.{NodeKind, Node_, SyntaxChecking}
import scala.util.control.Breaks._
import JavaSyntaxChecker.firstIndexOf

/**
 * Created by qwert on 29.04.15.
 */
object ArrayInitSyntax extends SyntaxChecking.SyntaxChecker(ArrayInitialize){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
    for(kid <- kids)
      if(!classOf[Expr].isInstance(kid.kind) || !classOf[ArrayInit].isInstance(kid.kind))
        error(s"All kids must be of kind Expr or ArrayInit, but found ${kid.kind.getClass}")

    if(kids.exists(!_.kind.isInstanceOf[Expr]))
      error(s"All kids must be of kind Expr, but found ${kids.filter(!_.kind.isInstanceOf[Expr])}")

    if(lits.nonEmpty)
      error(s"No literals allowed, but found ${lits.size}")
  }
}

object ArrayCreationSyntax extends SyntaxChecking.SyntaxChecker(NewArray){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
    if(lits.size != 1)
      error(s"Just one literal allowed, but found ${lits.size}")

    if(!classOf[ArrayBaseType].isInstance(lits.head))
      error(s"The first literal must be an ArrayBaseType, but was ${lits.head.getClass}")

    if(kids.isEmpty)
      error(s"Kids must be non empty")

    if(kids.head.kind.isInstanceOf[DimExpr.type ]) {
      // "new" ArrayBaseType DimExpr+ Dim*  -> ArrayCreationExpr {cons("NewArray")}
      // first elem that is not DimExpr
      var split: Int = -1
      breakable { for(i <- 1 until kids.size) {
        if(!kids(i).kind.isInstanceOf[DimExpr.type]){
          split = i
          break
        }
      } }

      if(split != -1) {
        for (i <- split until kids.size) {
          if (!kids(i).kind.isInstanceOf[Dim.type])
            error(s"All kids after the first Dim must be of type Dim, but found ${kids(i).kind.getClass}")
        }
      }
    } else if(kids.head.kind.isInstanceOf[Dim.type]) {
      // "new" ArrayBaseType Dim+ ArrayInit -> ArrayCreationExpr {cons("NewArray")}
      if(kids.dropRight(1).exists(!_.kind.isInstanceOf[Dim.type]))
        error(s"All kids except the last must be of kind Dim but found ${kids.dropRight(1).filter(!_.kind.isInstanceOf[Dim.type])}")

      if(!kids.last.kind.isInstanceOf[ArrayInit])
        error(s"The last kid must be of kind ArrayInit, but was ${kids.last.kind.getClass}")
    } else {
      error(s"The first kid must be of kind Dimension, but was ${kids.head.kind.getClass}")
    }
  }
}

object BlockSyntax extends SyntaxChecking.SyntaxChecker(Block){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    // stm syntax andAlso nolits syntax
    if(kids.exists(!_.kind.isInstanceOf[Stm]))
      error(s"All kids must be of kind Stm, but found ${kids.filter(!_.kind.isInstanceOf[Stm])}")

    if(lits.nonEmpty)
      error(s"No literals allowed, but found ${lits.size}")
  }
}

object FieldDecSyntax extends SyntaxChecking.SyntaxChecker(FieldDec){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    // TODO
  }
}

/*
object ArrayVarDecSyntax extends SyntaxChecking.SyntaxChecker(FieldDec){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(lits.size != 1)
      error(s"Just one literal allowed, but found ${lits.size}")

    if(!classOf[String].isInstance(lits(0)))
      error(s"The first literal must be an identifier, but was ${lits(0).getClass}")

    if(kids.size == 0)
      error(s"Kids must not be empty")

    if(kids.exists(!_.kind.isInstanceOf[Dimension]))
      error(s"All kids must be of kind Dim, but found ${kids.filter(!_.kind.isInstanceOf[Dimension])}")
  }
}

object ArrayVarDecInitSyntax extends SyntaxChecking.SyntaxChecker(FieldDec){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(lits.size != 1)
      error(s"Just one literal allowed, but found ${lits.size}")

    if(!classOf[String].isInstance(lits(0)))
      error(s"The first literal must be an identifier, but was ${lits(0).getClass}")

    if(kids.size == 0)
      error(s"Kids must not be empty")

    for(i <- 0 until kids.size-1)
      if(classOf[Dimension].isInstance(kids(i).kind))
        error(s"All kids but the last must be of kind Dim, but found ${kids(i).kind.getClass}")

    if(!classOf[Expr].isInstance(kids(kids.size-1)) || !classOf[ArrayInit].isInstance(kids(kids.size-1)))
      error(s"The last kid must be of kind Expr or ArrayInit, but was ${kids(kids.size-1).getClass}")
  }
}*/

case class FormalParamSyntax(k: NodeKind) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(!k.isInstanceOf[NT_FormalParam])
      error(s"FormalParamSyntax is just applicable to FormalParam, but was applied to ${k.getClass}")

    if(lits.size < 2)
      error(s"There must be at least 2 literals, but found only ${lits.size}")
    if(lits.dropRight(2).exists(!_.isInstanceOf[VarMod]))
      error(s"The first literals must be of kind VarMod, but found ${lits.dropRight(2).filter(!_.isInstanceOf[VarMod])}")
    if(!lits(lits.size-2).isInstanceOf[Type])
      error(s"The forelast literal must be of kind Type but was ${lits(lits.size-2).getClass}")
    if(!lits.last.isInstanceOf[NT_VarDecId])
      error(s"The last literal must be of kind VarDecId but was ${lits.last.getClass}")
    if(kids.exists(!_.kind.isInstanceOf[NT_Anno]))
      error(s"All kids must be of kind Anno, but found ${kids.filter(!_.kind.isInstanceOf[NT_Anno])}")
  }
}

object MethodInvokationSyntax extends SyntaxChecking.SyntaxChecker(Invoke) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(lits.nonEmpty)
      error(s"No literals allowed, but found ${lits.size}")

    if(kids.size < 1)
      error(s"At least 1 kid needed")

    if(!kids.head.kind.isInstanceOf[MethodSpec])
      error(s"First kind must be MethodSpec, but was ${kids.head.kind.getClass}")

    if(kids.size >= 2)
      for(i <- 1 until kids.size)
        if(!classOf[Expr].isInstance(kids(i).kind))
          error(s"All kids in argument position must be of kind Expr, but found ${kids(i).kind.getClass}")
  }
}

object MethodDecHeadSyntax extends SyntaxChecking.SyntaxChecker(MethodDeclarationHead) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    /* TODO: (Anno | MethodMod)*
     *
     */
    if(kids.exists(!_.kind.isInstanceOf[FormalParam]))
      error(s"All kids must be of kind FormalParam, but found ${kids.filter(!_.kind.isInstanceOf[FormalParam])}")
  }
}

object DeprMethodDecHeadSyntax extends SyntaxChecking.SyntaxChecker(DeprMethodDeclarationHead) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    // TODO: (Anno | MethodMod)*
  }
}

object ConstrDecHeadSyntax extends SyntaxChecking.SyntaxChecker(ConstrDecHead) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    // TODO: ( Anno | ConstrMod )* TypeParams? Id "(" {FormalParam ","}* ")" Throws?
  }
}

object ConstrBodySyntax extends SyntaxChecking.SyntaxChecker(ConstrBody) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    // TODO: "{" ConstrInv? BlockStm* "}"
  }
}

case class ConstrInvSyntax(k: NodeKind) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    // TODO: reject QSuperConstrInv (maybe split)

    // TODO: TypeArgs? "this" "(" {Expr ","}* ")" ";"
    // TODO: same for "super"
  }
}

object QConstrInvSyntax extends SyntaxChecking.SyntaxChecker(QSuperConstrInv) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    // TODO: Expr "." TypeArgs? "super" "(" {Expr ","}* ")" ";"
  }
}

object AbstractMethodDecSyntax extends SyntaxChecking.SyntaxChecker(AbstractMethodDec) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
  // ( Anno | AbstractMethodMod )* TypeParams? ResultType Id "(" {FormalParam ","}* ")" Throws? ";"
    // TODO: refactor: typeParamsPos and resultTypePos are statically known from syntax (because FormalParam is kid and not lit)
    if(lits.size < 2)
      error(s"There must be at least 2 literals, but found ${lits.size}")

    if(lits.count(_.isInstanceOf[TypeParams]) == 1) {
      // TypeParams
      val typeParamsPos = firstIndexOf(lits, classOf[TypeParams])

      // AbstractMethodMod*
      if(lits.slice(0, typeParamsPos).exists(!_.isInstanceOf[AbstractMethodMod]))
        error(s"All literals before TypeParams must be of kind AbstractMethodMod, but found ${lits.slice(0, typeParamsPos).filter(!_.isInstanceOf[AbstractMethodMod])}")

      if(lits.count(_.isInstanceOf[Throws]) == 1) {
        // Throws
        // TypeParams ResultType Id Throws
        if(typeParamsPos+3 > lits.size)
          error(s"TypeParams must not occur in the last three positions.")
        if(!classOf[ResultType].isInstance(lits(typeParamsPos+1)) && !classOf[String].isInstance(lits(typeParamsPos+2)) && !classOf[Throws].isInstance(lits(typeParamsPos+3)) && !classOf[Throws].isInstance(lits.last))
          error(s"The literal sequence must be TypeParams, ResultType, String, Throws but was TypeParams, ${lits(typeParamsPos+1).getClass}, ${lits(typeParamsPos+2).getClass}, ${lits(typeParamsPos+3).getClass}")
      } else {
        // no Throws
        // TypeParams ResultType Id
        if(typeParamsPos+2 > lits.size)
          error(s"TypeParams must not occur in the last two positions.")
        if(!classOf[ResultType].isInstance(lits(typeParamsPos+1)) && !classOf[String].isInstance(lits(typeParamsPos+2)))
          error(s"The literal sequence must be TypeParams, ResultType, String but was TypeParams, ${lits(typeParamsPos+1).getClass}, ${lits(typeParamsPos+2).getClass}")
      }
    } else {
      // no TypeParams
      val resultTypePos = firstIndexOf(lits, classOf[ResultType])

      if(lits.count(_.isInstanceOf[Throws]) == 1) {
        // Throws
        if(resultTypePos+2 > lits.size)
          error(s"ResultType must not occur in the last two positions")
        if(!classOf[String].isInstance(lits(resultTypePos+1)) && !classOf[Throws].isInstance(lits(resultTypePos+2)) && !classOf[Throws].isInstance(lits.last))
          error(s"The literal sequence must be ResultType, String, Throws but was ResultType, ${lits(resultTypePos+1).getClass}, ${lits(resultTypePos+2).getClass}")
      } else {
        // no Throws
        if(resultTypePos+1 > lits.size)
          error(s"ResultType must not occur in the last position")
        if(!classOf[String].isInstance(lits(resultTypePos+1)))
          error(s"The literal sequence must be ResultType, String but was ResultType, ${lits(resultTypePos+1).getClass}")
      }
    }
  }
}

object DeprAbstractMethodDecSyntax extends SyntaxChecking.SyntaxChecker(DeprAbstractMethodDec) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
  // TODO: ( Anno | AbstractMethodMod )* TypeParams? ResultType Id "(" {FormalParam ","}* ")" Dim+ Throws? ";"
  }
}

object ConstantDecSyntax extends SyntaxChecking.SyntaxChecker(ConstantDec) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
  // TODO: ( Anno | ConstantMod )* Type {VarDec ","}+ ";"
  }
}

object InterfaceDecHeadSyntax extends SyntaxChecking.SyntaxChecker(InterfaceDecHead) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    // TODO: ( Anno | InterfaceMod )* "interface" Id TypeParams? ExtendsInterfaces?
  }
}

object AnnoDecSyntax extends SyntaxChecking.SyntaxChecker(AnnoDec) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(lits.nonEmpty)
      error(s"No literals allowed, but found ${lits.size}")

    if(kids.isEmpty)
      error(s"Kids must not be empty")

    if(!kids.head.kind.isInstanceOf[NT_AnnoDecHead])
      error(s"First kid must be of Kind AnnoDecHead, but was ${kids.head.kind.getClass}")

    if(kids.drop(1).exists(!_.kind.isInstanceOf[NT_AnnoElemDec]))
      error(s"All kids but the first must be of kind AnnoElemDec, but found ${kids.drop(1).filter(!_.kind.isInstanceOf[NT_AnnoElemDec])}")
  }
}

object AnnoDecHeadSyntax extends SyntaxChecking.SyntaxChecker(AnnoDecHead) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    //
  }
}

object AnnoMethodDecSyntax extends SyntaxChecking.SyntaxChecker(AnnoMethodDec) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(lits.size < 2)
      error(s"At least two literals must exist, but found ${lits.size}")
    if(lits.dropRight(2).exists(!_.isInstanceOf[AbstractMethodMod]))
      error(s"Literals must be of kind AbstractMethodMod, but found ${lits.dropRight(1).filter(!_.isInstanceOf[AbstractMethodMod])}")
    if(!lits(lits.size-2).isInstanceOf[Type])
      error(s"The last literal must be of kind Type, but was ${lits.last.getClass}")
    if(!lits.last.isInstanceOf[String])
      error(s"The last literal must be of kind Id, but was ${lits.last.getClass}")
    if(kids.size != 1)
      error(s"There must be exact one kid, but found ${kids.size}")
    if(!kids.head.kind.isInstanceOf[NT_DefaultVal])
      error(s"The kid must be of type DefaultVal but was ${kids.head.kind.getClass}")
  }
}

object JavaSyntaxChecker {
  def noLits = (k: NodeKind) => new NoLitsSyntax(k)
  def nonEmptyLits = (k: NodeKind) => new NonEmptyLitsSyntax(k)
  def lits(litTypes: Seq[Class[_]]) = (k: NodeKind) => new LitsSyntax(k, litTypes)
  def allLits(litsType: Class[_]) = (k: NodeKind) => new LitsSequenceSyntax(k, litsType)
  def noKids = (k: NodeKind) => new NoKidsSyntax(k)
  def kids(kidTypes: Seq[Class[_ <: NodeKind]]) = (k: NodeKind) => new KidsSyntax(k, kidTypes)
  def allKids(kidsType: Class[_ <: NodeKind]) = (k: NodeKind) => new KidsSequenceSyntax(k, kidsType)
  def unsafeKids(kidTypes: Seq[Class[_]]) = (k: NodeKind) => new UnsafeKidsSyntax(k, kidTypes)
  def unsafeAllKids(kidsType: Class[_]) = (k: NodeKind) => new UnsafeKidsSequenceSyntax(k, kidsType)
  //def followedByKids(first: Class[_ <: NodeKind], tail: Class[_ <: NodeKind]) = (k: NodeKind) => new KidFollowedByKidsSyntax(k, first, tail)
  //def kidsFollowedBy(head: Class[_ <: NodeKind], last: Class[_ <: NodeKind]) = (k: NodeKind) => new KidsFollowedByKidSyntax(k, head, last)
  def followedByLits(first: Class[_], tail: Class[_]) = (k: NodeKind) => new LitFollowedByLitsSyntax(k, first, tail)
  def litsFollowedBy(head: Class[_], last: Class[_]) = (k: NodeKind) => new LitsFollowedByLitSyntax(k, head, last)
  //def exprKids = (k: NodeKind) => new ExprKindsSyntax(k)
  //def stmKids = (k: NodeKind) => new StmKindsSyntax(k)
  //def exprOrStmKids = (k: NodeKind) => new ExprOrStmKindsSyntax(k)
  def exprKids = allKids(classOf[Expr])
  def stmKids = allKids(classOf[Stm])
  def exprOrStmKids = (k: NodeKind) => new ExprOrStmKindsSyntax(k)


  def firstIndexOf(list: Seq[_], elem: Class[_]): Int = {
    for(i: Int <- list.indices)
      if(elem.isInstance(list(i)))
        return i
    return 0
  }
}

/*case class ExprKindsSyntax(k: NodeKind) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(kids.exists(!_.kind.isInstanceOf[Expr]))
      error(s"All kids must be of kind Expr, but found ${kids.filter(!_.kind.isInstanceOf[Expr])}")
  }
}

case class StmKindsSyntax(k: NodeKind) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(kids.exists(!_.kind.isInstanceOf[Stm]))
      error(s"All kids must be of kind Stm, but found ${kids.filter(!_.kind.isInstanceOf[Stm])}")
  }
}*/

case class ExprOrStmKindsSyntax(k: NodeKind) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    for(kid <- kids)
      if(!classOf[Expr].isInstance(kid.kind) || !classOf[Stm].isInstance(kid.kind))
        error(s"All kids must be of kind Stm or Expr, but found ${kid.kind.getClass}")
  }
}

case class NoKidsSyntax(k: NodeKind) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(kids.nonEmpty)
      error(s"No kids allowed, but found ${kids.size}")
  }
}

case class KidsSyntax(k: NodeKind, kidTypes: Seq[Class[_ <: NodeKind]]) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(kids.size != kidTypes.size)
      error(s"Expected ${kidTypes.size} kids but found ${kids.size} kids")

    for(i <- kids.indices)
      if(!kidTypes(i).isInstance(kids(i).kind))
        error(s"Expected kid of kind ${kidTypes(i)} at position $i but found ${kids(i)} of kind ${kids(i).kind.getClass}")
  }
}

case class KidsSequenceSyntax(k: NodeKind, kidsType: Class[_ <: NodeKind]) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    for(kid <- kids)
      if(!kidsType.isInstance(kid.kind))
        error(s"All kids must be of kind $kidsType, but found ${kid.kind.getClass}")
  }
}

case class UnsafeKidsSyntax(k: NodeKind, kidTypes: Seq[Class[_]]) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(kids.size != kidTypes.size)
      error(s"Expected ${kidTypes.size} kids but found ${kids.size} kids")

    for(i <- kids.indices)
      if(!kidTypes(i).isInstance(kids(i).kind))
        error(s"Expected kid of kind ${kidTypes(i)} at position $i but found ${kids(i)} of kind ${kids(i).kind.getClass}")
  }
}

case class UnsafeKidsSequenceSyntax(k: NodeKind, kidsType: Class[_]) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    for(kid <- kids)
      if(!kidsType.isInstance(kid.kind))
        error(s"All kids must be of kind $kidsType, but found ${kid.kind.getClass}")
  }
}

case class NoLitsSyntax(k: NodeKind) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(lits.nonEmpty)
      error(s"No literals allowed, but found ${lits.size}")
  }
}

case class NonEmptyLitsSyntax(k: NodeKind) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(lits.isEmpty)
      error(s"Literals must not be empty")
  }
}

case class LitsSyntax(k: NodeKind, litTypes: Seq[Class[_]]) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(lits.size != litTypes.size)
      error(s"Expected ${litTypes.size} literals but found ${lits.size} literals")

    for (i <- lits.indices)
      if (!litTypes(i).isInstance(lits(i)))
        error(s"Expected literal of ${litTypes(i)} at position $i but found ${lits(i)} of ${lits(i).getClass}")
  }
}

case class LitsSequenceSyntax(k: NodeKind, litsType: Class[_])extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    for(lit <- lits)
      if(!litsType.isInstance(lit))
        error(s"All lits must be of kind $litsType, but found ${lit.getClass}")
  }
}

/*
case class KidsFollowedByKidSyntax(k: NodeKind, headsType: Class[_ <: NodeKind], lastType: Class[_ <: NodeKind]) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(kids.isEmpty)
      error(s"At least one kid needed")

    if(!lastType.isInstance(kids.last.kind))
      error(s"The last kid must be of kind $lastType, but was ${kids.last.kind.getClass}")

    if(kids.dropRight(1).exists(!_.kind.isInstanceOf[headsType.type]))
      error(s"Alls kids but the last must be of kind $headsType, but found ${kids.dropRight(1).filter(!_.kind.isInstanceOf[headsType.type])}")
  }
}

case class KidFollowedByKidsSyntax(k: NodeKind, firstType: Class[_ <: NodeKind], tailType: Class[_ <: NodeKind]) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(kids.isEmpty)
      error(s"At least one kid needed")

    if(!firstType.isInstance(kids.head.kind))
      error(s"The first kid must be of kind $firstType, but was ${kids.head.kind}")

    if(kids.drop(1).exists(!_.kind.isInstanceOf[tailType.type]))
      error(s"Alls kids but the first must be of kind $tailType, but found ${kids.dropRight(1).filter(!_.kind.isInstanceOf[tailType.type])}")
  }
}
*/

case class LitsFollowedByLitSyntax(k: NodeKind, headsType: Class[_], lastType: Class[_]) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(lits.isEmpty)
      error(s"At least one literal needed")

    if(!lastType.isInstance(lits.last))
      error(s"The last literal must be of kind $lastType, but was ${lits.last.getClass}")

    for(lit <- lits.dropRight(1))
      if(!headsType.isInstance(lit))
        error(s"All literals but the last must be of kind $headsType, but found ${lit.getClass}")
  }
}

case class LitFollowedByLitsSyntax(k: NodeKind, firstType: Class[_], tailType: Class[_]) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(lits.isEmpty)
      error(s"At least one literal needed")

    if(!firstType.isInstance(lits.head))
      error(s"The first literal must be of kind $firstType, but was ${lits.last.getClass}")

    for(lit <- lits.drop(1))
      if(!tailType.isInstance(lit))
        error(s"All literals but the first must be of kind $tailType, but found ${lit.getClass}")
  }
}