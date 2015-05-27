package incremental.Java.syntax

import incremental.Node._
import incremental.{NodeKind, Node_, SyntaxChecking}

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

    if(lits.size != 0)
      error(s"No literals allowed, but found ${lits.size}")
  }
}

object ArrayCreationSyntax extends SyntaxChecking.SyntaxChecker(NewArray){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
    if(lits.size != 1)
      error(s"Just one literal allowed, but found ${lits.size}")

    if(!classOf[ArrayBaseType].isInstance(lits(0)))
      error(s"The first literal must be an ArrayBaseType, but was ${lits(0).getClass}")

    if(kids.size >= 1 && kids.exists(!_.kind.isInstanceOf[Dimension]))
      error(s"All kids must be of kind Dimension, but found ${kids.filter(!_.kind.isInstanceOf[Dimension])}")
    else if(kids.size >= 2 && !kids.last.isInstanceOf[ArrayInit] && kids.dropRight(1).exists(!_.kind.isInstanceOf[Dim.type ]))
      error(s"The last kid must be of type ArrayInit, but was ${kids.last.getClass}")
  }
}

object BlockSyntax extends SyntaxChecking.SyntaxChecker(Block){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    // stm syntax andAlso nolits syntax
    if(kids.exists(!_.kind.isInstanceOf[Stm]))
      error(s"All kids must be of kind Stm, but found ${kids.filter(!_.kind.isInstanceOf[Stm])}")

    if(lits.size != 0)
      error(s"No literals allowed, but found ${lits.size}")
  }
}

object FieldDecSyntax extends SyntaxChecking.SyntaxChecker(FieldDec){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    // TODO
  }
}

object ArrayVarDecIdSyntax extends SyntaxChecking.SyntaxChecker(ArrayVarDecId) {
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

object MethodInvokationSyntax extends SyntaxChecking.SyntaxChecker(Invoke) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(lits.size != 0)
      error(s"No literals allowed, but found ${lits.size}")

    if(kids.size < 1)
      error(s"At least 1 kid needed")

    if(!kids(0).kind.isInstanceOf[MethodSpec])
      error(s"First kind must be MethodSpec, but was ${kids(0).kind.getClass}")

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

object JavaSyntaxChecker {
  def noLits = (k: NodeKind) => new NoLitsSyntax(k)
  def lits(litTypes: Seq[Class[_]]) = (k: NodeKind) => new LitsSyntax(k, litTypes)
  def noKids = (k: NodeKind) => new NoKidsSyntax(k)
  def kids(kidTypes: Seq[Class[_ <: NodeKind]]) = (k: NodeKind) => new KidsSyntax(k, kidTypes)
  def allKids(kidsType: Class[_ <: NodeKind]) = (k: NodeKind) => new KidsSequenceSyntax(k, kidsType)
  def oneFollowedByMany(first: Class[_ <: NodeKind], tail: Class[_ <: NodeKind]) = (k: NodeKind) => new KidFollowedByKidsSyntax(k, first, tail)
  def manyFollowedByOne(head: Class[_ <: NodeKind], last: Class[_ <: NodeKind]) = (k: NodeKind) => new KidsFollowedByKidSyntax(k, head, last)
  //def exprKids = (k: NodeKind) => new ExprKindsSyntax(k)
  //def stmKids = (k: NodeKind) => new StmKindsSyntax(k)
  //def exprOrStmKids = (k: NodeKind) => new ExprOrStmKindsSyntax(k)
  def exprKids = allKids(classOf[Expr])
  def stmKids = allKids(classOf[Stm])
  def exprOrStmKids = (k: NodeKind) => new ExprOrStmKindsSyntax(k)
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
    if(kids.size != 0)
      error(s"No kids allowed, but found ${kids.size}")
  }
}

case class KidsSyntax(k: NodeKind, kidTypes: Seq[Class[_ <: NodeKind]]) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(kids.size != kidTypes.size)
      error(s"Expected ${kidTypes.size} kids but found ${kids.size} kids")

    for(i <- 0 until kids.size)
      if(!kidTypes(i).isInstance(kids(i).kind))
        error(s"Expected kid of kind ${kidTypes(i)} at position $i but found ${kids(i)} of kind ${kids(i).kind.getClass}")
  }
}

case class KidsSequenceSyntax(k: NodeKind, kidsType: Class[_ <: NodeKind]) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    for(kid <- kids)
      if(!kidsType.isInstance(kid.kind))
        error(s"All kids must be of kind ${kidsType}, but found ${kid.kind.getClass}")
  }
}

case class NoLitsSyntax(k: NodeKind) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(lits.size != 0)
      error(s"No literals allowed, but found ${lits.size}")
  }
}

case class LitsSyntax(k: NodeKind, litTypes: Seq[Class[_]]) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(lits.size != litTypes.size)
      error(s"Expected ${litTypes.size} literals but found ${lits.size} literals")

    for (i <- 0 until lits.size)
      if (!litTypes(i).isInstance(lits(i)))
        error(s"Expected literal of ${litTypes(i)} at position $i but found ${lits(i)} of ${lits(i).getClass}")
  }
}

case class KidsFollowedByKidSyntax(k: NodeKind, headsType: Class[_ <: NodeKind], lastType: Class[_ <: NodeKind]) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(kids.size < 1)
      error(s"At least one kid needed")

    if(!lastType.isInstance(kids.last.kind))
      error(s"The last kid must be of kind ${lastType}, but was ${kids.last.kind}")

    if(kids.dropRight(1).exists(!_.kind.isInstanceOf[headsType.type]))
      error(s"Alls kids but the last must be of kind ${headsType}, but found ${kids.dropRight(1).filter(!_.kind.isInstanceOf[headsType.type])}")
  }
}

case class KidFollowedByKidsSyntax(k: NodeKind, firstType: Class[_ <: NodeKind], tailType: Class[_ <: NodeKind]) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(kids.size < 1)
      error(s"At least one kid needed")

    if(!firstType.isInstance(kids(0).kind))
      error(s"The first kid must be of kind ${firstType}, but was ${kids(0).kind}")

    if(kids.drop(1).exists(!_.kind.isInstanceOf[tailType.type]))
      error(s"Alls kids but the last must be of kind ${tailType}, but found ${kids.dropRight(1).filter(!_.kind.isInstanceOf[tailType.type])}")
  }
}