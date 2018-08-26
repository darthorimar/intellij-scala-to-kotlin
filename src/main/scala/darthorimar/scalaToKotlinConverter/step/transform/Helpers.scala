package darthorimar.scalaToKotlinConverter.step.transform

import darthorimar.scalaToKotlinConverter.ast._
import darthorimar.scalaToKotlinConverter.types.TypeUtils.WithType

object Helpers {

  object ApplyCall {
    def unapply(expr: Expr): Option[(Expr, Seq[Expr])] = expr match {
      case CallExpr(_, RefExpr(_, Some(obj), "apply", _, _), params, _) =>
        Some.apply((obj, params))
      case _ => None
    }

    object InfixCall {
      def unapply(expr: Expr): Option[(String, Expr, Expr, Type)] = expr match {
        case CallExpr(returnType, RefExpr(_, Some(firstArg), name, _, _), Seq(secondArg), _) =>
          Some((name, firstArg, secondArg, returnType))
        case _ => None
      }
    }

    object PrefixCall {
      def unapply(expr: Expr): Option[(String, Expr, Type)] = expr match {
        case CallExpr(_, RefExpr(returnType, Some(arg), name, _, _), Seq(), _) =>
          Some((name, arg, returnType))
        case _ => None
      }
    }

  }

}
