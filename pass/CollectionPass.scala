package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter
import org.jetbrains.plugins.kotlinConverter.{Exprs, Utils}
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.types.{KotlinTypes, TypeUtils}

class CollectionPass extends Pass {

  override def pass[T](ast: AST): T = {
    //    if (ast.isInstanceOf[FileDef])
    //      println(Utils.prettyPrint(ast))
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
    case CallExpr(_, RefExpr(_, Some(RefExpr(_, None, "Seq" | "List", _, false)), "empty", typeParams, _), Seq()) =>
      if (typeParams.isEmpty) Some(Exprs.emptyList)
      else Some(Exprs.emptyList(pass[Type](typeParams.head.ty)))

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

    // seq.mkString(a,b,c) --> seq.joinToString(b,a,c)
    case CallExpr(ty, RefExpr(refTy, Some(obj), "mkString", typeParams, true), params)
      if TypeUtils.isKotlinList(obj.ty) =>
      val newParams =
        if (params.length == 3) Seq(params(1), params(0), params(2))
        else params
      Some(CallExpr(ty, RefExpr(refTy, Some(pass[Expr](obj)), "joinToString", typeParams, true), newParams))

    // seq.tail --> seq.drop(1)
    case CallExpr(ty, RefExpr(refTy, Some(obj), "tail", typeParams, true), _)
      if TypeUtils.isKotlinList(obj.ty) =>
      Some(CallExpr(ty, RefExpr(refTy, Some(pass[Expr](obj)), "drop", typeParams, true), Seq(LitExpr(KotlinTypes.INT, "1"))))

    // seq.init --> seq.dropLast(1)
    case CallExpr(ty, RefExpr(refTy, Some(obj), "init", typeParams, true), _)
      if TypeUtils.isKotlinList(obj.ty) =>
      Some(CallExpr(ty, RefExpr(refTy, Some(pass[Expr](obj)), "dropLast", typeParams, true), Seq(LitExpr(KotlinTypes.INT, "1"))))

    //seq.foreach --> seq.forEach
    case CallExpr(ty, RefExpr(refTy, Some(obj), "foreach", typeParams, true), params)
      if TypeUtils.isKotlinList(obj.ty) =>
      Some(CallExpr(ty, RefExpr(refTy, Some(pass[Expr](obj)), "forEach", typeParams, true), params.map(pass[Expr])))

    // str * i => str.repeat(i)
    case BinExpr(ty, "*", left, right) if left.ty == KotlinTypes.STRING && right.ty == KotlinTypes.INT =>
      Some(CallExpr(ty, RefExpr(ty, Some(pass[Expr](left)), "repeat", Seq.empty, true), Seq(right)))

    // seq(i) --> seq[i]
    case CallExpr(ty, obj, Seq(index))
      if TypeUtils.isKotlinList(obj.ty) =>
        Some(BracketsExpr(ty, pass[Expr](obj), pass[Expr](index)))

    case RefExpr(refTy, Some(obj), "asInstanceOf", Seq(TypeParam(ty)), false) =>
      Some(ParenExpr(Exprs.as(pass[Expr](obj), pass[Type](ty))))

    case RefExpr(refTy, Some(obj), "isInstanceOf", Seq(TypeParam(ty)), false) =>
      Some(ParenExpr(Exprs.is(pass[Expr](obj), pass[Type](ty))))

    case _ => None
  }
}