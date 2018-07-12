package org.jetbrains.plugins.kotlinConverter.ast

import scala.meta.Enumerator.Guard

trait AST

//case object EmptyAst extends AST with Expr with Attr with CasePattern with BlockExpr {
//  override def stmts: Seq[Expr] = Seq.empty
//  override def ty: Type = NoType
//
//  override def name: String = "NoName"
//}

case class DefParam(ty: Type, name: String) extends AST
case class MatchCaseClause(pattern: MatchCasePattern, expr: Expr, guard: Option[Expr]) extends AST
//case class WhenCaseClause(pattern: WhenCasePattern, expr: Expr) extends AST



sealed trait Construct extends AST
case class ParamsConstruct(params: Seq[ConstructParam]) extends Construct
case object EmptyConstruct extends Construct

case class ConstructParam(parType: MemberKind, mod: Attr, name: String, ty: Type) extends AST

case class TypeParam(ty: String) extends AST

case class Super(ty: Type, construct: Option[Construct]) extends AST
case class FileDef(pckg: String, imports: Seq[ImportDef], defns: Seq[DefExpr]) extends AST

case class BinOp(name: String) extends AST

sealed trait MatchCasePattern extends AST

case class LitPatternMatch(lit: LitExpr) extends MatchCasePattern
case class ConstructorPatternMatch(ref: String, args: Seq[MatchCasePattern])  extends MatchCasePattern
case class TypedPatternMatch(ref: String, ty: Type) extends MatchCasePattern
case class ReferencePatternMatch(ref: String) extends MatchCasePattern
case object WildcardPatternMatch extends MatchCasePattern

sealed trait WhenClause extends AST

case class ExprWhenClause(clause: Expr, expr: Expr) extends WhenClause
case class ElseWhenClause(expr: Expr) extends WhenClause

trait Destructor extends AST {
  def name: String
}

case class LitDestructor(lit: LitExpr) extends Destructor {
  override def name: String = lit.name
}
case class RefDestructor(ref: String) extends Destructor {
  override def name: String = ref
}
case object WildcardDestructor extends Destructor {
  override def name: String = "_"
}
