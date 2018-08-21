package darthorimar.scalaToKotlinConverter

import darthorimar.scalaToKotlinConverter.ast.{BlockExpr, Expr}
import darthorimar.scalaToKotlinConverter.types.{KotlinTypes, StdTypes}

object ExprUtils {
  def blockOf(exprs: Seq[Expr]): BlockExpr = {
    val blockType =
      if(exprs.nonEmpty) exprs.last.exprType
      else StdTypes.NOTHING
    BlockExpr(blockType, exprs)
  }

}
