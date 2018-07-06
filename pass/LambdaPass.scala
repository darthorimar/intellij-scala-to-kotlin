package org.jetbrains.plugins.kotlinConverter.pass

import android.databinding.tool.expr.LambdaExpr
import org.jetbrains.plugins.kotlinConverter.ast.{AST, Expr}
import org.jetbrains.plugins.kotlinConverter.ast.Expr.Lambda
import org.jetbrains.plugins.kotlinConverter.ast.Stmt.{Block, MultiBlock, SingleBlock}

class LambdaPass extends Pass {
  override protected def action(ast: AST): Option[AST] = ast match {
    case MultiBlock(stmts) if stmts.size == 1 && stmts.head.isInstanceOf[Lambda] =>
      Some(SingleBlock(pass[Expr](stmts.head)))
    case _ => None
  }
}

