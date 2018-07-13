package org.jetbrains.plugins.kotlinConverter.ast

import org.jetbrains.plugins.kotlinConverter.ast

sealed trait Expr extends AST {
  def ty: Type
}

case class BinExpr(ty: Type, op: BinOp, left: Expr, right: Expr) extends Expr
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
case class IfExpr(ty: Type, cond: Expr, trueB: Expr, falseB: Expr) extends Expr
case class ForExpr(ty: Type, range: Expr, body: BlockExpr) extends Expr
case class WhileExpr(ty: Type, cond: Expr, body: BlockExpr) extends Expr
case class TypeExpr(ty: Type) extends Expr

sealed trait BlockExpr extends Expr {
  def stmts: Seq[Expr]
  def isSingle: Boolean = stmts.size == 1
  def isEmpty: Boolean = stmts.isEmpty
}
case class SingleBlock(stmt: Expr) extends BlockExpr {
  override def stmts: Seq[Expr] = Seq(stmt)

  override def ty: Type = stmt.ty
}
case class MultiBlock(stmts: Seq[Expr]) extends BlockExpr {
  override def ty: Type =
    if (isEmpty) NoType
    else stmts.last.ty
}

case object EmptyBlock extends  BlockExpr {
  override def stmts: Seq[Expr] = Seq.empty

  override def ty: Type = NoType
}

sealed trait DefExpr extends Expr
case class Defn(attrs: Seq[Attr],
                t: DefnType,
                name: String,
                construct: Option[Construct],
                supers: Seq[Super],
                block: BlockExpr) extends DefExpr {
  override def ty: Type = NoType
}
case class ValDef(destructors: Seq[Destructor], ty: Type, expr: Expr) extends DefExpr
case class VarDef(name: String, ty: Type, expr: Expr) extends DefExpr
case class DefnDef(attrss: Seq[Attr], name: String, ty: Type, args: Seq[DefParam], retType: Type, body: BlockExpr) extends DefExpr
case class ImportDef(ref: String, names: Seq[String]) extends DefExpr {
  override def ty: Type = NoType
}


