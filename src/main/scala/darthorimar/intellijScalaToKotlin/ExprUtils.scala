package darthorimar.intellijScalaToKotlin

import darthorimar.intellijScalaToKotlin.ast.{BlockExpr, Expr}
import darthorimar.intellijScalaToKotlin.types.{KotlinTypes, StdTypes}

object ExprUtils {
  def blockOf(exprs: Seq[Expr]): BlockExpr = {
    val blockType =
      if(exprs.nonEmpty) exprs.last.exprType
      else StdTypes.NOTHING
    BlockExpr(blockType, exprs)
  }

}
