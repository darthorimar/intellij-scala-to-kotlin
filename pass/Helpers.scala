package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.ast._

object Helpers {
  object ApplyCall {
    def unapply(expr: Expr): Option[(Expr, Seq[Expr])] = expr match {
      case CallExpr(_, RefExpr(_, Some(obj), "apply", _, _), params, _) =>
        Some((obj, params))
      case _ => None
    }
  }
}
