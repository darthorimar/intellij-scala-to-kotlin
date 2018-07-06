package org.jetbrains.plugins.kotlinConverter.pass

import android.databinding.tool.expr.LambdaExpr
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.ast.Expr.Lambda
import org.jetbrains.plugins.kotlinConverter.ast.Stmt.{Block, ClassDef, MultiBlock, SingleBlock}

class BasicPass extends Pass {
  override protected def action(ast: AST): Option[AST] = ast match {
    //Remove renault brackets for lambda like in seq.map {x => x * 2}
    case MultiBlock(stmts) if stmts.size == 1 && stmts.head.isInstanceOf[Lambda] =>
      Some(SingleBlock(pass[Expr](stmts.head)))
    case ParamsConstruct(params)
      if parent.asInstanceOf[ClassDef].attrs.contains(CaseAttr) =>
        Some(ParamsConstruct(params.map {
          case ConstructParam(parType, mod, name, ty) =>
            val t = if (parType == NoType) ValType else parType
            val m = if (mod == NoModifier) PublModifier else mod
            ConstructParam(t, m, name, pass[Type](ty))
        }))
    case _ => None
  }
}

