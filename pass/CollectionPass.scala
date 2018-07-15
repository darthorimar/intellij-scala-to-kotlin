package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter
import org.jetbrains.plugins.kotlinConverter.Exprs
import org.jetbrains.plugins.kotlinConverter.ast._

class CollectionPass extends Pass {
  override protected def action(ast: AST): Option[AST] = ast match {
    // Some(x) --> x
    case CallExpr(NullableType(_), RefExpr(_, None, "Some", _, _), Seq(v)) =>
      Some(pass[Expr](v))

    // opt.map(x => x + 1) --> opt?.let {x => x + 1}
    case CallExpr(ty, RefExpr(refTy, Some(obj), "map", typeParams, true), Seq(p))
      if obj.ty.isInstanceOf[NullableType] =>
      Some(CallExpr(
        pass[Type](ty),
        RefExpr(pass[Type](refTy),
          Some(kotlinConverter.ast.PostExpr(obj.ty, pass[Expr](obj), "?")),
          "let",
          typeParams.map(pass[TypeParam]), true),
        Seq(pass[Expr](p))))

    case CallExpr(ty, RefExpr(refTy, Some(obj), "getOrElse", _, true), Seq(p)) if obj.ty.isInstanceOf[NullableType] =>
      Some(BinExpr(pass[Type](refTy), BinOp("?:"), obj, pass[Expr](p)))

    //opt.get --> opt!!
    case RefExpr(refTy, Some(obj), "get", _, true)
      if obj.ty.isInstanceOf[NullableType] =>
      Some(PostExpr(pass[Type](refTy), pass[Expr](obj), "!!"))

    //Seq(1,2,3) --> listOf(1,2,3)
    case CallExpr(ty, RefExpr(refTy, None, "Seq", typeParams, _), params) =>
      Some(CallExpr(
        pass[Type](ty),
        RefExpr(pass[Type](refTy), None, "listOf", typeParams.map(pass[TypeParam]), true),
        params.map(pass[Expr])))


    case RefExpr(refTy, Some(obj), "asInstanceOf", Seq(TypeParam(ty)), false) =>
      Some(ParenExpr(Exprs.as(pass[Expr](obj), pass[Type](ty))))

    case RefExpr(refTy, Some(obj), "isInstanceOf", Seq(TypeParam(ty)), false) =>
      Some(ParenExpr(Exprs.is(pass[Expr](obj), pass[Type](ty))))

    case _ => None
  }
}

