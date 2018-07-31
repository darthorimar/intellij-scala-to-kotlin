package org.jetbrains.plugins.kotlinConverter

import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.types.KotlinTypes

object Exprs {
  def is(expr: Expr, ty: Type) =
    BinExpr(KotlinTypes.BOOLEAN, "is", expr, TypeExpr(ty))

  def as(expr: Expr, ty: Type) =
    BinExpr(KotlinTypes.BOOLEAN, "as", expr, TypeExpr(ty))

  def and(left: Expr, right: Expr) =
    BinExpr(KotlinTypes.BOOLEAN, "&&", left, right)

  def letExpr(obj: Expr, lambda: LambdaExpr) =
    CallExpr(lambda.exprType, RefExpr(NoType, Some(obj), "let", Seq.empty, true), Seq(lambda))

  def emptyList(ty: Type) =
    CallExpr(
      listType(ty),
      RefExpr(ty, None, "emptyList", Seq(TypeParam(ty)), true),
      Seq.empty)

  def emptyList =
    CallExpr(
      listType(NoType),
      RefExpr(NoType, None, "emptyList", Seq.empty, true),
      Seq.empty)

  def listType(ty: Type) =
    GenerecTypes(KotlinTypes.LIST, Seq(ty))


  val falseLit = LitExpr(KotlinTypes.BOOLEAN, "false")
  val trueLit = LitExpr(KotlinTypes.BOOLEAN, "true")
  val nullLit = LitExpr(NoType, "null") //TODO fix
}
