package org.jetbrains.plugins.kotlinConverter

import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.types.KotlinTypes

object Exprs {
  def is(expr: Expr, ty: Type) =
    BinExpr(KotlinTypes.BOOLEAN, BinOp("is"), expr, TypeExpr(ty))

  def and(left: Expr, right: Expr) =
    BinExpr(KotlinTypes.BOOLEAN, BinOp("&&"), left, right)

  def letExpr(obj: Expr, lambda: LambdaExpr) =
    CallExpr(lambda.ty, RefExpr(NoType, Some(obj), "let", Seq.empty, true), Seq(lambda))

  val falseLit = LitExpr(KotlinTypes.BOOLEAN, "false")
  val trueLit = LitExpr(KotlinTypes.BOOLEAN, "true")
  val nullLit = LitExpr(NoType, "null") //TODO fix
}
