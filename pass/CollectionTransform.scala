package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter
import org.jetbrains.plugins.kotlinConverter.{Exprs, Utils}
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.types.{KotlinTypes, TypeUtils}

class CollectionTransform extends Transform {

  override def pass[T](ast: AST): T = {
    super.pass(ast)
  }

  override protected def action(ast: AST): Option[AST] = ast match {
    //Options

    // Some(x) --> x
    case CallExpr(_, RefExpr(_, Some(RefExpr(_, None, "Some", _, false)), "apply", _, _), Seq(v)) =>
      Some(pass[Expr](v))

    // None --> null
    case RefExpr(SimpleType("scala.None.type"), None, "None", _, _) =>
      Some(Exprs.nullLit)

    // opt.map(f), opt.flatMap(f) --> opt?.let {f(it)}
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "map" | "flatMap", typeParams, true), Seq(p))
      if referenceObject.exprType.isInstanceOf[NullableType] =>
      Some(CallExpr(
        pass[Type](exprType),
        RefExpr(pass[Type](refTy),
          Some(kotlinConverter.ast.PostfixExpr(referenceObject.exprType, pass[Expr](referenceObject), "?")),
          "let",
          typeParams.map(pass[TypeParam]), true),
        Seq(pass[Expr](p))))

    // opt.getOrElse(x) --> opt :? x
    case CallExpr(_, RefExpr(refTy, Some(referenceObject), "getOrElse", _, true), Seq(p)) if referenceObject.exprType.isInstanceOf[NullableType] =>
      Some(BinExpr(pass[Type](refTy), "?:", referenceObject, pass[Expr](p)))

    //opt.get --> opt!!
    case CallExpr(_, RefExpr(refTy, Some(referenceObject), "get", _, true), _)
      if referenceObject.exprType.isInstanceOf[NullableType] =>
      Some(PostfixExpr(pass[Type](refTy), pass[Expr](referenceObject), "!!"))

    //Seqs

    //Seq(1,2,3) --> listOf(1,2,3)
    case CallExpr(exprType, RefExpr(refTy, Some(RefExpr(_, None, "Seq", typeParams, false)), "apply", _, _), params) =>
      Some(CallExpr(
        pass[Type](exprType),
        RefExpr(pass[Type](refTy), None, "listOf", typeParams.map(pass[TypeParam]), true),
        params.map(pass[Expr])))

    //Seq.empty[T] --> emptyList<T>()
    case CallExpr(_, RefExpr(_, Some(RefExpr(_, None, "Seq" | "List", _, false)), "empty", typeParams, _), Seq()) =>
      if (typeParams.isEmpty) Some(Exprs.emptyList)
      else Some(Exprs.emptyList(pass[Type](typeParams.head.parameterType)))

    //Nil --> emptytList()
    case RefExpr(SimpleType("scala.Nil.type" | "scala.collection.immutable.Nil.type"), None, "Nil", _, false) =>
      Some(Exprs.emptyList)

    // (1 :: seq, 1 +: seq)  --> listOf(1) + seq
    case BinExpr(GenerecTypes(KotlinTypes.LIST, Seq(exprType)), "::" | "+:", left, right) =>
      Some(
        BinExpr(Exprs.listType(pass[Type](exprType)),
          "+",
          CallExpr(
            pass[Type](exprType),
            RefExpr(pass[Type](exprType), None, "listOf", Seq.empty, true),
            Seq(pass[Expr](left))),
          pass[Expr](right)))

    // seq :+ 1  --> seq + 1
    case BinExpr(GenerecTypes(KotlinTypes.LIST, Seq(exprType)), ":+", left, right) =>
      Some(
        BinExpr(Exprs.listType(pass[Type](exprType)),
          "+",
          pass[Expr](right),
          pass[Expr](right)))

    // seq.mkString(a,b,c) --> seq.joinToString(b,a,c)
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "mkString", typeParams, true), params)
      if TypeUtils.isKotlinList(referenceObject.exprType) =>
      val newParams =
        if (params.length == 3) Seq(params(1), params(0), params(2))
        else params
      Some(CallExpr(exprType, RefExpr(refTy, Some(pass[Expr](referenceObject)), "joinToString", typeParams, true), newParams))

    // seq.tail --> seq.drop(1)
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "tail", typeParams, true), _)
      if TypeUtils.isKotlinList(referenceObject.exprType) =>
      Some(CallExpr(exprType, RefExpr(refTy, Some(pass[Expr](referenceObject)), "drop", typeParams, true), Seq(LitExpr(KotlinTypes.INT, "1"))))

    // seq.init --> seq.dropLast(1)
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "init", typeParams, true), _)
      if TypeUtils.isKotlinList(referenceObject.exprType) =>
      Some(CallExpr(exprType, RefExpr(refTy, Some(pass[Expr](referenceObject)), "dropLast", typeParams, true), Seq(LitExpr(KotlinTypes.INT, "1"))))

    //seq.foreach --> seq.forEach
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "foreach", typeParams, true), params)
      if TypeUtils.isKotlinList(referenceObject.exprType) =>
      Some(CallExpr(exprType, RefExpr(refTy, Some(pass[Expr](referenceObject)), "forEach", typeParams, true), params.map(pass[Expr])))

    // str * i => str.repeat(i)
    case BinExpr(exprType, "*", left, right) if left.exprType == KotlinTypes.STRING && right.exprType == KotlinTypes.INT =>
      Some(CallExpr(exprType, RefExpr(exprType, Some(pass[Expr](left)), "repeat", Seq.empty, true), Seq(right)))

    // seq(i) --> seq[i]
//    case CallExpr(exprType, refExpr, Seq(index))
//      if TypeUtils.isKotlinList(refExpr.exprType) =>
//        Some(BracketsExpr(exprType, pass[Expr](refExpr), pass[Expr](index)))

    case RefExpr(refTy, Some(referenceObject), "asInstanceOf", Seq(TypeParam(exprType)), false) =>
      Some(ParenthesesExpr(Exprs.as(pass[Expr](referenceObject), pass[Type](exprType))))

    case RefExpr(refTy, Some(referenceObject), "isInstanceOf", Seq(TypeParam(exprType)), false) =>
      Some(ParenthesesExpr(Exprs.is(pass[Expr](referenceObject), pass[Type](exprType))))

    case _ => None
  }
}