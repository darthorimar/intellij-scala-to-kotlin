package org.jetbrains.plugins.kotlinConverter.ast

trait AST

case object EmptyAst extends AST with Expr with Attr with CasePattern with BlockExpr {
  override def stmts: Seq[Expr] = Seq.empty
  override def ty: Type = NoType

  override def name: String = "NoName"
}

case class DefParam(ty: Type, name: String) extends AST
case class CaseClause(pattern: CasePattern, expr: Expr) extends AST


trait CasePattern extends AST
case class LitPattern(lit: LitExpr) extends CasePattern
case class ConstructorPattern(ref: String, args: Seq[CasePattern])  extends CasePattern
case class TypedPattern(ref: String, ty: Type) extends CasePattern
case class ReferencePattern(ref: String) extends CasePattern
case object WildcardPattern extends CasePattern


trait Construct extends AST
case class ParamsConstruct(params: Seq[ConstructParam]) extends Construct
case object EmptyConstruct extends Construct

case class ConstructParam(parType: MemberKind, mod: Attr, name: String, ty: Type) extends AST

case class TypeParam(ty: String) extends AST

case class Super(ty: Type, construct: Option[Construct]) extends AST
case class FileDef(pckg: String, imports: Seq[ImportDef], defns: Seq[DefExpr]) extends AST

case class BinOp(name: String) extends AST
