package org.jetbrains.plugins.kotlinConverter

import org.jetbrains.plugins.kotlinConverter.ast.{BlockExpr, Expr}
import org.jetbrains.plugins.kotlinConverter.types.{KotlinTypes, StdTypes}

object ExprUtils {
  def blockOf(exprs: Seq[Expr]): BlockExpr = {
    val blockType =
      if(exprs.nonEmpty) exprs.last.exprType
      else StdTypes.NOTHING
    BlockExpr(blockType, exprs)
  }

}
