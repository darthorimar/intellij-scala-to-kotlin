package org.jetbrains.plugins.kotlinConverter.ast

trait AST


case class DefParam(ty: Type, name: String) extends AST
case class MatchCaseClause(pattern: MatchCasePattern, expr: Expr, guard: Option[Expr]) extends AST



sealed trait Construct extends AST
case class ParamsConstruct(params: Seq[ConstructParam]) extends Construct
case object EmptyConstruct extends Construct

case class ConstructParam(parType: MemberKind, mod: Attr, name: String, ty: Type) extends AST

case class TypeParam(ty: Type) extends AST

case class FileDef(pckg: String, imports: Seq[ImportDef], defns: Seq[DefExpr]) extends AST

sealed trait MatchCasePattern extends AST {
  def name: String
}

case class CompositePatternMatch(parts: Seq[MatchCasePattern]) extends MatchCasePattern {
  override def name: String = parts.mkString(" | ")
}
case class LitPatternMatch(expr: Expr) extends MatchCasePattern {
  override def name: String = expr match {
    case LitExpr(_, name) => name
    case RefExpr(_, _, ref, _, _) => ref
  }
}
case class ConstructorPatternMatch(ref: String, args: Seq[MatchCasePattern], label: Option[String], repr: String)  extends MatchCasePattern {
  override def name: String = repr
}
case class TypedPatternMatch(ref: String, ty: Type) extends MatchCasePattern {
  override def name: String = s"$ref: $ty"
}
case class ReferencePatternMatch(ref: String) extends MatchCasePattern {
  override def name: String = ref
}
case object WildcardPatternMatch extends MatchCasePattern {
  override def name: String = "_"
}

sealed trait WhenClause extends AST

case class ExprWhenClause(clause: Expr, expr: Expr) extends WhenClause
case class ElseWhenClause(expr: Expr) extends WhenClause

sealed trait ForEnumerator extends  AST
case class ForGenerator(pattern: MatchCasePattern, expr: Expr) extends ForEnumerator
case class ForGuard(condition: Expr) extends ForEnumerator
case class ForVal(pattern: MatchCasePattern, expr: Expr) extends ForEnumerator

case class SupersBlock(constuctor: Option[SuperConstructor], supers: Seq[Type]) extends AST

case class SuperConstructor(ty: Type, exprs: Seq[Expr]) extends AST
