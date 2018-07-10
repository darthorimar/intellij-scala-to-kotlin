package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.types._

class CollectionPass extends Pass {
  override protected def action(ast: AST): Option[AST] = ast match {
    case InvExpr(ty@FuncType(_, NullableType(_)), obj, "map")  =>
      Some(InvExpr(pass[Type](ty), obj.map(x => pass[Expr](PostExpr(pass[Type](ty), x, "?"))), "let"))

//    case CallExpr(_,InvExpr(ty, Some(obj), "get"), _, _) if obj.ty.isInstanceOf[NullableType] =>
//      Some(PostExpr(pass[Type](ty), pass[Expr](obj), "!!"))
//
//    case CallExpr(FuncType(_, t),InvExpr(ty, Some(obj), "getOrElse"), _, Seq(p)) if obj.ty.isInstanceOf[NullableType] =>
//      Some(BinExpr(pass[Type](t), BinOp("?:"), obj, pass[Expr](p)))

    case _ => None
  }

}

