package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter
import org.jetbrains.plugins.kotlinConverter.{Exprs, Utils}
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.types.KotlinTypes

class CollectionPass extends Pass {

  override def pass[T](ast: AST): T = {
    if (ast.isInstanceOf[FileDef])
      println(Utils.prettyPrint(ast))
    super.pass(ast)
  }

  override protected def action(ast: AST): Option[AST] = ast match {
    //Options

    // Some(x) --> x
    case CallExpr(NullableType(_), RefExpr(_, None, "Some", _, _), Seq(v)) =>
      Some(pass[Expr](v))

    // None --> null
    case CallExpr(NullableType(_), RefExpr(_, None, "None", _, _), Seq()) =>
      Some(Exprs.nullLit)

    // opt.map(f), opt.flatMap(f) --> opt?.let {f(it)}
    case CallExpr(ty, RefExpr(refTy, Some(obj), "map" | "flatMap", typeParams, true), Seq(p))
      if obj.ty.isInstanceOf[NullableType] =>
      Some(CallExpr(
        pass[Type](ty),
        RefExpr(pass[Type](refTy),
          Some(kotlinConverter.ast.PostExpr(obj.ty, pass[Expr](obj), "?")),
          "let",
          typeParams.map(pass[TypeParam]), true),
        Seq(pass[Expr](p))))

    // opt.getOrElse(x) --> opt :? x
    case CallExpr(_, RefExpr(refTy, Some(obj), "getOrElse", _, true), Seq(p)) if obj.ty.isInstanceOf[NullableType] =>
      Some(BinExpr(pass[Type](refTy), "?:", obj, pass[Expr](p)))

    //opt.get --> opt!!
    case RefExpr(refTy, Some(obj), "get", _, true)
      if obj.ty.isInstanceOf[NullableType] =>
      Some(PostExpr(pass[Type](refTy), pass[Expr](obj), "!!"))

    //Seqs

    //Seq(1,2,3) --> listOf(1,2,3)
    case CallExpr(ty, RefExpr(refTy, None, "Seq", typeParams, _), params) =>
      Some(CallExpr(
        pass[Type](ty),
        RefExpr(pass[Type](refTy), None, "listOf", typeParams.map(pass[TypeParam]), true),
        params.map(pass[Expr])))

    //Seq.empty[T] --> emptyList<T>()
    case CallExpr(_, RefExpr(_, Some(RefExpr(_, None, "Seq" | "List", _, false)), "empty", Seq(TypeParam(ty)), _), Seq()) =>
      Some(Exprs.emptyList(pass[Type](ty)))

    //Nil --> emptytList()
    case RefExpr(SimpleType("scala.Nil.type"), None, "Nil", _, false) =>
      Some(Exprs.emptyList)

    // (1 :: seq, 1 +: seq)  --> listOf(1) + seq
    case BinExpr(ProductType(KotlinTypes.LIST, Seq(ty)), "::" | "+:", left, right) =>
      Some(
        BinExpr(Exprs.listType(pass[Type](ty)),
          "+",
          CallExpr(
            pass[Type](ty),
            RefExpr(pass[Type](ty), None, "listOf", Seq.empty, true),
            Seq(pass[Expr](left))),
          pass[Expr](right)))

    // seq :+ 1  --> seq + 1
    case BinExpr(ProductType(KotlinTypes.LIST, Seq(ty)), ":+", left, right) =>
      Some(
        BinExpr(Exprs.listType(pass[Type](ty)),
          "+",
          pass[Expr](right),
          pass[Expr](right)))

    case RefExpr(refTy, Some(obj), "asInstanceOf", Seq(TypeParam(ty)), false) =>
      Some(ParenExpr(Exprs.as(pass[Expr](obj), pass[Type](ty))))

    case RefExpr(refTy, Some(obj), "isInstanceOf", Seq(TypeParam(ty)), false) =>
      Some(ParenExpr(Exprs.is(pass[Expr](obj), pass[Type](ty))))

    case _ => None
  }
}