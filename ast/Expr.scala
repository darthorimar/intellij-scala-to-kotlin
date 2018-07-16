package org.jetbrains.plugins.kotlinConverter.ast

import org.jetbrains.plugins.kotlinConverter.ast
import org.jetbrains.plugins.kotlinConverter.types.KotlinTypes

sealed trait Expr extends AST {
  def ty: Type
}

case class BinExpr(ty: Type, op: String, left: Expr, right: Expr) extends Expr
case class ParenExpr(inner: Expr) extends Expr {
  override def ty: Type = inner.ty
}
case class CallExpr(ty: Type, ref: Expr, params: Seq[Expr]) extends Expr
case class LitExpr(ty: Type, name: String) extends Expr
case class UnderScExpr(ty: Type) extends Expr
case class RefExpr(ty: Type, obj: Option[Expr], ref: String, typeParams: Seq[TypeParam], isFunc: Boolean)
  extends Expr
case class PostExpr(ty: Type, obj: Expr, op: String) extends Expr
case class MatchExpr(ty: Type, expr: Expr, clauses: Seq[MatchCaseClause]) extends Expr
case class WhenExpr(ty: Type, expr: Option[Expr], clauses: Seq[WhenClause]) extends Expr
case class AssignExpr(left: Expr, right: Expr) extends Expr {
  override def ty: Type = SimpleType("Unit")
}
case class NewExpr(ty: Type, name: String, args: Seq[Expr]) extends Expr
case class LambdaExpr(ty: Type, params: Seq[DefParam], expr: Expr, needBraces: Boolean) extends Expr
case class ThrowExpr(ty: Type, expr: Expr) extends Expr
case class IfExpr(ty: Type, cond: Expr, trueB: Expr, falseB: Option[Expr]) extends Expr
case class ForExpr(ty: Type, generators: Seq[ForGenerator], body: Expr) extends Expr
case class WhileExpr(ty: Type, cond: Expr, body: BlockExpr) extends Expr
case class ReturnExpr(label: Option[String], expr: Option[Expr]) extends Expr {
  override def ty: Type = KotlinTypes.NOTHING
}
case class TryExpr(tryBlock: Expr, finallyBlock: Option[Expr]) extends Expr {
  override def ty: Type = tryBlock.ty
}
case class TypeExpr(ty: Type) extends Expr

case class BlockExpr(ty: Type, exprs: Seq[Expr]) extends Expr {
  def isSingle: Boolean = exprs.size == 1
  def isEmpty: Boolean = exprs.isEmpty
}


sealed trait DefExpr extends Expr
case class Defn(attrs: Seq[Attr],
                t: DefnType,
                name: String,
                typeParams: Seq[TypeParam],
                construct: Option[Construct],
                supers: Seq[Super],
                body: Option[Expr]) extends DefExpr {
  override def ty: Type = NoType
}
case class ValDef(destructors: Seq[MatchCasePattern], expr: Expr) extends DefExpr {
  override def ty: Type = KotlinTypes.NOTHING
}
case class LazyValDef(name: String, ty: Type, expr: Expr) extends DefExpr
case class VarDef(name: String, ty: Type, expr: Expr) extends DefExpr
case class DefnDef(attrs: Seq[Attr],
                   name: String,
                   typeParams: Seq[TypeParam],
                   ty: Type,
                   args: Seq[DefParam],
                   retType: Type,
                   body: Option[Expr]) extends DefExpr
case class ImportDef(ref: String, names: Seq[String]) extends DefExpr {
  override def ty: Type = NoType
}


