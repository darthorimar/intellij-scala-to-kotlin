package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.types._

class CollectionPass extends Pass {
  override protected def action(ast: AST): Option[AST] = ast match {
    case CallExpr(ty@FuncType(_, NullableType(_)), _, "Some",_, Seq(v)) =>
      Some(pass[Expr](v))

    case CallExpr(ty@FuncType(_, NullableType(_)), Some(obj), "map", _, Seq(p)) =>
      Some(CallExpr(pass[Type](ty),
        Some(pass[Expr](PostExpr(pass[Type](ty), pass[Expr](obj), "?"))),
        "let",
        Seq.empty,
        Seq(pass[Expr](p))))

    case CallExpr(ty, Some(obj), "get", _, _) =>
      Some(PostExpr(pass[Type](ty), pass[Expr](obj), "!!"))
    //
    //    case CallExpr(FuncType(_, t),InvExpr(ty, Some(obj), "getOrElse"), _, Seq(p)) if obj.ty.isInstanceOf[NullableType] =>
    //      Some(BinExpr(pass[Type](t), BinOp("?:"), obj, pass[Expr](p)))

    case _ => None
  }

}

