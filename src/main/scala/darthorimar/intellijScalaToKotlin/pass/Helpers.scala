package darthorimar.intellijScalaToKotlin.pass

import darthorimar.intellijScalaToKotlin.ast._

object Helpers {
  object ApplyCall {
    def unapply(expr: Expr): Option[(Expr, Seq[Expr])] = expr match {
      case CallExpr(_, RefExpr(_, Some(obj), "apply", _, _), params, _) =>
        Some.apply((obj, params))
      case _ => None
    }
  }
}
