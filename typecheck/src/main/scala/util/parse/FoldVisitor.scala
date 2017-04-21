package util.parse

import com.github.javaparser.ast._
import com.github.javaparser.ast.`type`._
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.comments.{BlockComment, JavadocComment, LineComment}
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.modules._
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.visitor.{GenericListVisitorAdapter, VoidVisitor, VoidVisitorAdapter}

/**
  * Created by oliver on 21.04.17.
  */
trait NodeVisitor extends VoidVisitorAdapter[Unit] {
  
  def node(n: Node): Unit = {}

  override def visit(n: NodeList[_ <: Node], arg: Unit): Unit = { super.visit(n, arg) }

  override def visit(n: AnnotationDeclaration, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: AnnotationMemberDeclaration, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ArrayAccessExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ArrayCreationExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ArrayCreationLevel, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ArrayInitializerExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ArrayType, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: AssertStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: AssignExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: BinaryExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: BlockComment, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: BlockStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: BooleanLiteralExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: BreakStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: CastExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: CatchClause, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: CharLiteralExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ClassExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ClassOrInterfaceDeclaration, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ClassOrInterfaceType, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: CompilationUnit, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ConditionalExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ConstructorDeclaration, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ContinueStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: DoStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: DoubleLiteralExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: EnclosedExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: EnumConstantDeclaration, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: EnumDeclaration, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ExplicitConstructorInvocationStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ExpressionStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: FieldAccessExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: FieldDeclaration, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ForStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ForeachStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: IfStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ImportDeclaration, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: InitializerDeclaration, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: InstanceOfExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: IntegerLiteralExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: IntersectionType, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: JavadocComment, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: LabeledStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: LambdaExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: LineComment, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: LocalClassDeclarationStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: LongLiteralExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: MarkerAnnotationExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: MemberValuePair, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: MethodCallExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: MethodDeclaration, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: MethodReferenceExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: NameExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: Name, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: NormalAnnotationExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: NullLiteralExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ObjectCreationExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: PackageDeclaration, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: Parameter, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: PrimitiveType, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ReturnStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: SimpleName, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: SingleMemberAnnotationExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: StringLiteralExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: SuperExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: SwitchEntryStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: SwitchStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: SynchronizedStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ThisExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ThrowStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: TryStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: TypeExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: TypeParameter, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: UnaryExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: UnionType, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: UnknownType, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: VariableDeclarationExpr, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: VariableDeclarator, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: VoidType, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: WhileStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: WildcardType, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ModuleDeclaration, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ModuleRequiresStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ModuleExportsStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ModuleProvidesStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ModuleUsesStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

  override def visit(n: ModuleOpensStmt, arg: Unit): Unit = { super.visit(n, arg); node(n) }

}


trait FoldVisitor[S] extends NodeVisitor {
  var res: S = init()

  def init(): S

  def fold(n: Node, s: S): S

  override def node(n: Node): Unit = {
    super.node(n)
    res = fold(n, res)
  }
}

trait PrintlnVisitor extends NodeVisitor {
  override def node(n: Node): Unit = {
    super.node(n)
    println(s"${n.getClass.getSimpleName}: $n")
  }
}

import java.util.{List => JList}
/**
  * Created by oliver on 21.04.17.
  */
trait NodeListVisitor[R] extends GenericListVisitorAdapter[R, Unit] {

  def join(n: Node, l: JList[R]): JList[R]

  override def visit(n: NodeList[_ <: Node], arg: Unit): JList[R] = { super.visit(n, arg) }

  override def visit(n: AnnotationDeclaration, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: AnnotationMemberDeclaration, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ArrayAccessExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ArrayCreationExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ArrayCreationLevel, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ArrayInitializerExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ArrayType, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: AssertStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: AssignExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: BinaryExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: BlockComment, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: BlockStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: BooleanLiteralExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: BreakStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: CastExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: CatchClause, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: CharLiteralExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ClassExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ClassOrInterfaceDeclaration, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ClassOrInterfaceType, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: CompilationUnit, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ConditionalExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ConstructorDeclaration, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ContinueStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: DoStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: DoubleLiteralExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: EnclosedExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: EnumConstantDeclaration, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: EnumDeclaration, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ExplicitConstructorInvocationStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ExpressionStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: FieldAccessExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: FieldDeclaration, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ForStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ForeachStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: IfStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ImportDeclaration, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: InitializerDeclaration, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: InstanceOfExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: IntegerLiteralExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: IntersectionType, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: JavadocComment, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: LabeledStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: LambdaExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: LineComment, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: LocalClassDeclarationStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: LongLiteralExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: MarkerAnnotationExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: MemberValuePair, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: MethodCallExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: MethodDeclaration, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: MethodReferenceExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: NameExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: Name, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: NormalAnnotationExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: NullLiteralExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ObjectCreationExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: PackageDeclaration, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: Parameter, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: PrimitiveType, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ReturnStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: SimpleName, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: SingleMemberAnnotationExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: StringLiteralExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: SuperExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: SwitchEntryStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: SwitchStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: SynchronizedStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ThisExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ThrowStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: TryStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: TypeExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: TypeParameter, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: UnaryExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: UnionType, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: UnknownType, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: VariableDeclarationExpr, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: VariableDeclarator, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: VoidType, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: WhileStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: WildcardType, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ModuleDeclaration, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ModuleRequiresStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ModuleExportsStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ModuleProvidesStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ModuleUsesStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

  override def visit(n: ModuleOpensStmt, arg: Unit): JList[R] = { join(n, super.visit(n, arg)) }

}