package org.jetbrains.plugins.kotlinConverter

import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.types.KotlinTypes

object Exprs {
  def is(expr: Expr, ty: Type) =
    BinExpr(KotlinTypes.BOOLEAN, BinOp("is"), expr, TypeExpr(ty))
  def and(left: Expr, right: Expr) =
    BinExpr(KotlinTypes.BOOLEAN, BinOp("&&"), left, right)
}
