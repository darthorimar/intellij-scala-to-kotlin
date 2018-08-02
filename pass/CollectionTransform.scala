package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter
import org.jetbrains.plugins.kotlinConverter.{Exprs, Utils}
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.types.{KotlinTypes, TypeUtils}

class CollectionTransform extends Transform {

  override def transform[T](ast: AST): T = {
    super.transform(ast)
  }

  override protected def action(ast: AST): Option[AST] = ast match {
    //Options

    // Some(x) --> x
    case CallExpr(_, RefExpr(_, Some(RefExpr(_, None, "Some", _, false)), "apply", _, _), Seq(v)) =>
      Some(transform[Expr](v))

    // None --> null
    case RefExpr(SimpleType("scala.None.type"), None, "None", _, _) =>
      Some(Exprs.nullLit)

    // opt.map(f), opt.flatMap(f) --> opt?.let {f(it)}
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "map" | "flatMap", typeParams, true), Seq(p))
      if referenceObject.exprType.isInstanceOf[NullableType] =>
      Some(CallExpr(
        transform[Type](exprType),
        RefExpr(transform[Type](refTy),
          Some(kotlinConverter.ast.PostfixExpr(referenceObject.exprType, transform[Expr](referenceObject), "?")),
          "let",
          typeParams.map(transform[TypeParam]), true),
        Seq(transform[Expr](p))))

    // opt.getOrElse(x) --> opt :? x
    //    case CallExpr(_, RefExpr(refTy, Some(referenceObject), "getOrElse", _, true), Seq(p)) if referenceObject.exprType.isInstanceOf[NullableType] =>
    //      Some(InfixExpr(transform[Type](refTy), "?:", referenceObject, transform[Expr](p)))

    //opt.get --> opt!!
    case CallExpr(_, RefExpr(refTy, Some(referenceObject), "get", _, true), _)
      if referenceObject.exprType.isInstanceOf[NullableType] =>
      Some(PostfixExpr(transform[Type](refTy), transform[Expr](referenceObject), "!!"))

    //Seqs

    //Seq(1,2,3) --> listOf(1,2,3)
    case CallExpr(exprType, RefExpr(refTy, Some(RefExpr(_, None, "Seq", typeParams, false)), "apply", _, _), params) =>
      Some(CallExpr(
        transform[Type](exprType),
        RefExpr(transform[Type](refTy), None, "listOf", typeParams.map(transform[TypeParam]), true),
        params.map(transform[Expr])))

    //Seq.empty[T] --> emptyList<T>()
    case CallExpr(_, RefExpr(_, Some(RefExpr(_, None, "Seq" | "List", _, false)), "empty", typeParams, _), Seq()) =>
      if (typeParams.isEmpty) Some(Exprs.emptyList)
      else Some(Exprs.emptyList(transform[Type](typeParams.head.parameterType)))

    //Nil --> emptytList()
    case RefExpr(SimpleType("scala.Nil.type" | "scala.collection.immutable.Nil.type"), None, "Nil", _, false) =>
      Some(Exprs.emptyList)

    //     (1 :: seq, 1 +: seq)  --> listOf(1) + seq
    case CallExpr(exprType, RefExpr(refTy, Some(left), "::" | "+:", _, _), Seq(right)) =>
//      if TypeUtils.isKotlinList(right.exprType) =>
      Some(Exprs.simpleInfix(
        Exprs.listType(transform[Type](exprType)),
        "+",
        CallExpr(
          transform[Type](exprType),
          RefExpr(transform[Type](exprType), None, "listOf", Seq.empty, true),
          Seq(transform[Expr](left))),
        transform[Expr](right)))


    // seq :+ 1  --> seq + 1
    case CallExpr(exprType, RefExpr(refTy, Some(left), ":+", _, true), Seq(right)) =>
      Some(
        Exprs.simpleInfix(Exprs.listType(transform[Type](exprType)),
          "+",
          transform[Expr](right),
          transform[Expr](right)))

    // seq.mkString(a,b,c) --> seq.joinToString(b,a,c)
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "mkString", typeParams, true), params)
      if TypeUtils.isKotlinList(referenceObject.exprType) =>
      val newParams =
        if (params.length == 3) Seq(params(1), params(0), params(2))
        else params
      Some(CallExpr(exprType, RefExpr(refTy, Some(transform[Expr](referenceObject)), "joinToString", typeParams, true), newParams))

    // seq.tail --> seq.drop(1)
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "tail", typeParams, true), _)
      if TypeUtils.isKotlinList(referenceObject.exprType) =>
      Some(CallExpr(exprType, RefExpr(refTy, Some(transform[Expr](referenceObject)), "drop", typeParams, true), Seq(LitExpr(KotlinTypes.INT, "1"))))

    // seq.init --> seq.dropLast(1)
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "init", typeParams, true), _)
      if TypeUtils.isKotlinList(referenceObject.exprType) =>
      Some(CallExpr(exprType, RefExpr(refTy, Some(transform[Expr](referenceObject)), "dropLast", typeParams, true), Seq(LitExpr(KotlinTypes.INT, "1"))))

    //seq.foreach --> seq.forEach
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "foreach", typeParams, true), params)
      if TypeUtils.isKotlinList(referenceObject.exprType) =>
      Some(CallExpr(exprType, RefExpr(refTy, Some(transform[Expr](referenceObject)), "forEach", typeParams, true), params.map(transform[Expr])))

    //     str * i => str.repeat(i)
    case CallExpr(exprType, RefExpr(refTy, Some(left), "*", _, _), Seq(right))
      if left.exprType == KotlinTypes.STRING && right.exprType == KotlinTypes.INT =>
      Some(CallExpr(exprType, RefExpr(exprType, Some(transform[Expr](left)), "repeat", Seq.empty, true), Seq(right)))

    // seq(i) --> seq[i]
    //    case CallExpr(exprType, refExpr, Seq(index))
    //      if TypeUtils.isKotlinList(refExpr.exprType) =>
    //        Some(BracketsExpr(exprType, pass[Expr](refExpr), pass[Expr](index)))

    case RefExpr(refTy, Some(referenceObject), "asInstanceOf", Seq(TypeParam(exprType)), false) =>
      Some(ParenthesesExpr(Exprs.as(transform[Expr](referenceObject), transform[Type](exprType))))

    case RefExpr(refTy, Some(referenceObject), "isInstanceOf", Seq(TypeParam(exprType)), false) =>
      Some(ParenthesesExpr(Exprs.is(transform[Expr](referenceObject), transform[Type](exprType))))

    case _ => None
  }
}