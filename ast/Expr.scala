package org.jetbrains.plugins.kotlinConverter.ast

import org.jetbrains.plugins.kotlinConverter.ast
import org.jetbrains.plugins.kotlinConverter.types.KotlinTypes

sealed trait Expr extends AST {
  def exprType: Type
}

case class BinExpr(exprType: Type, op: String, left: Expr, right: Expr) extends Expr
case class ParenthesesExpr(inner: Expr) extends Expr {
  override def exprType: Type = inner.exprType
}
case class CallExpr(exprType: Type, ref: Expr, params: Seq[Expr]) extends Expr
case class LitExpr(exprType: Type, name: String) extends Expr
case class UnderscoreExpr(exprType: Type) extends Expr
case class RefExpr(exprType: Type,
                   referencedObject: Option[Expr],
                   referenceName: String,
                   typeParams: Seq[TypeParam],
                   isFunctionRef: Boolean)
  extends Expr
case class PostfixExpr(exprType: Type, expr: Expr, op: String) extends Expr
case class MatchExpr(exprType: Type, expr: Expr, clauses: Seq[MatchCaseClause]) extends Expr
case class WhenExpr(exprType: Type, expr: Option[Expr], clauses: Seq[WhenClause]) extends Expr
case class BracketsExpr(exprType: Type, expr: Expr, inBrackets: Expr) extends Expr
case class AssignExpr(left: Expr, right: Expr) extends Expr {
  override def exprType: Type = SimpleType("Unit")
}
case class NewExpr(exprType: Type, name: String, arguments: Seq[Expr]) extends Expr
case class LambdaExpr(exprType: Type, parameters: Seq[DefParameter], expr: Expr, needBraces: Boolean) extends Expr
case class ThrowExpr(exprType: Type, expr: Expr) extends Expr
case class IfExpr(exprType: Type, condition: Expr, trueBranch: Expr, falseBranch: Option[Expr]) extends Expr
case class ForExpr(exprType: Type, generators: Seq[ForEnumerator], body: Expr) extends Expr
case class ForInExpr(exprType: Type, value: RefExpr, range: Expr, body: Expr) extends Expr
case class WhileExpr(exprType: Type, condition: Expr, body: BlockExpr) extends Expr
case class ThisExpr(exprType: Type) extends Expr
case class InterpolatedStringExpr(parts: Seq[String], injected: Seq[Expr]) extends Expr {
  override def exprType: Type = KotlinTypes.STRING
}
case class ReturnExpr(label: Option[String], expr: Option[Expr]) extends Expr {
  override def exprType: Type = KotlinTypes.NOTHING
}
case class TryExpr(tryBlock: Expr, finallyBlock: Option[Expr]) extends Expr {
  override def exprType: Type = tryBlock.exprType
}
case class TypeExpr(exprType: Type) extends Expr

case class BlockExpr(exprType: Type, exprs: Seq[Expr]) extends Expr {
  def isSingle: Boolean = exprs.size == 1
  def isEmpty: Boolean = exprs.isEmpty
}


sealed trait DefExpr extends Expr
case class Defn(attrs: Seq[Attribute],
                defnType: DefnType,
                name: String,
                typeParams: Seq[TypeParam],
                constructor: Option[Constructor],
                supersBlock: Option[SupersBlock],
                body: Option[Expr]) extends DefExpr {
  override def exprType: Type = NoType
}
case class ValDef(destructors: Seq[CasePattern], expr: Expr) extends DefExpr {
  override def exprType: Type = KotlinTypes.NOTHING
}
case class LazyValDef(name: String, exprType: Type, expr: Expr) extends DefExpr
case class VarDef(name: String, exprType: Type, expr: Expr) extends DefExpr
case class DefnDef(attributes: Seq[Attribute],
                   name: String,
                   typeParameters: Seq[TypeParam],
                   exprType: Type,
                   parameters: Seq[DefParameter],
                   returnType: Type,
                   body: Option[Expr]) extends DefExpr
case class ImportDef(ref: String, names: Seq[String]) extends DefExpr {
  override def exprType: Type = NoType
}


