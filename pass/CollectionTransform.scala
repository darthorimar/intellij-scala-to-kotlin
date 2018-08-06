package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter
import org.jetbrains.plugins.kotlinConverter.{Exprs, Utils}
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.types.TypeUtils.{ListType, WithType}
import org.jetbrains.plugins.kotlinConverter.types.{KotlinTypes, TypeUtils}
import org.scalafmt.internal.SyntacticGroup.Term

class CollectionTransform extends Transform {

  override def transform[T](ast: AST): T = {
    //        if (ast.isInstanceOf[FileDef])
    //          println(Utils.prettyPrint(ast))
    super.transform(ast)
  }

  override protected def action(ast: AST): Option[AST] = ast match {
    //Options

    // Some(x) --> x
    case CallExpr(_, RefExpr(_, Some(RefExpr(_, None, "Some", _, false)), "apply", _, _), Seq(v), paramsExpectedTypes) =>
      Some(transform[Expr](v))

    // None --> null
    case RefExpr(ScalaCollectionType("scala.None$"), None, "None", _, _) =>
      Some(Exprs.nullLit)

    // opt.map(f), opt.flatMap(f) --> opt?.let {f(it)}
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "map" | "flatMap", typeParams, true), Seq(p), paramsExpectedTypes)
      if referenceObject.exprType.isInstanceOf[NullableType] =>
      Some(CallExpr(
        transform[Type](exprType),
        RefExpr(transform[Type](refTy),
          Some(kotlinConverter.ast.PostfixExpr(referenceObject.exprType, transform[Expr](referenceObject), "?")),
          "let",
          typeParams.map(transform[Type]), true),
        Seq(transform[Expr](p)),
        paramsExpectedTypes.map(transform[Type])))

    //     opt.getOrElse(x) --> opt :? x
    case CallExpr(_, RefExpr(refTy, Some(referenceObject@WithType(NullableType(_))), "getOrElse", _, true), Seq(p), paramsExpectedTypes)
      if referenceObject.exprType.isInstanceOf[NullableType] =>
      Some(Exprs.simpleInfix(transform[Type](refTy), ":?", transform[Expr](referenceObject), transform[Expr](p)))

    //opt.get --> opt!!
    case CallExpr(_, RefExpr(refTy, Some(referenceObject), "get", _, true), _, paramsExpectedTypes)
      if referenceObject.exprType.isInstanceOf[NullableType] =>
      Some(PostfixExpr(transform[Type](refTy), transform[Expr](referenceObject), "!!"))

    //Seqs

    //Seq(1,2,3) --> listOf(1,2,3)
    case CallExpr(exprType, RefExpr(refTy, Some(RefExpr(_, None, "Seq", typeParams, false)), "apply", _, _), params, paramsExpectedTypes) =>
      Some(CallExpr(
        transform[Type](exprType),
        RefExpr(transform[Type](refTy), None, "listOf", typeParams.map(transform[Type]), true),
        params.map(transform[Expr]),
        paramsExpectedTypes.map(transform[Type])))

    //Seq.empty[T] --> emptyList<T>()
    case CallExpr(_, RefExpr(_, Some(RefExpr(_, None, "Seq" | "List", _, false)), "empty", typeParams, _), Seq(), paramsExpectedTypes) =>
      if (typeParams.isEmpty) Some(Exprs.emptyList)
      else Some(Exprs.emptyList(transform[Type](typeParams.head)))

    //Nil --> emptytList()
    case RefExpr(ScalaCollectionType("scala.collection.immutable.Nil$"), None, "Nil", _, false) =>
      Some(Exprs.emptyList)

    //     (1 :: seq, 1 +: seq)  --> listOf(1) + seq
    case CallExpr(exprType, RefExpr(refTy, Some(left), "::" | "+:", _, _), Seq(right), paramsExpectedTypes) =>
      //      if TypeUtils.isKotlinList(right.exprType) =>
      Some(Exprs.simpleInfix(
        Exprs.listType(transform[Type](exprType)),
        "+",
        CallExpr(
          transform[Type](exprType),
          RefExpr(transform[Type](exprType), None, "listOf", Seq.empty, true),
          Seq(transform[Expr](left)),
          paramsExpectedTypes.map(transform[Type])),
        transform[Expr](right)))


    // seq :+ 1  --> seq + 1
    case CallExpr(exprType, RefExpr(refTy, Some(left), ":+", _, true), Seq(right), paramsExpectedTypes) =>
      Some(
        Exprs.simpleInfix(Exprs.listType(transform[Type](exprType)),
          "+",
          transform[Expr](right),
          transform[Expr](right)))

    // seq.mkString(a,b,c) --> seq.joinToString(b,a,c)
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "mkString", typeParams, true), params, paramsExpectedTypes)
      if TypeUtils.isKotlinList(referenceObject.exprType) =>
      val newParams =
        if (params.length == 3) Seq(params(1), params(0), params(2))
        else params
      Some(CallExpr(exprType,
        RefExpr(refTy, Some(transform[Expr](referenceObject)),
          "joinToString",
          typeParams, true),
        newParams,
        paramsExpectedTypes.map(transform[Type])))

    // seq.tail --> seq.drop(1)
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "tail", typeParams, true), _, paramsExpectedTypes)
      if TypeUtils.isKotlinList(referenceObject.exprType) =>
      Some(CallExpr(exprType,
        RefExpr(refTy, Some(transform[Expr](referenceObject)), "drop", typeParams, true),
        Seq(LitExpr(KotlinTypes.INT, "1")),
        paramsExpectedTypes.map(transform[Type])))

    // seq.head --> seq.first
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "head", typeParams, true), _, paramsExpectedTypes)
      if TypeUtils.isKotlinList(referenceObject.exprType) =>
      Some(CallExpr(exprType, RefExpr(refTy, Some(transform[Expr](referenceObject)), "first", typeParams, true), Seq.empty,
        paramsExpectedTypes.map(transform[Type])))


    // seq.init --> seq.dropLast(1)
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "init", typeParams, true), _, paramsExpectedTypes)
      if TypeUtils.isKotlinList(referenceObject.exprType) =>
      Some(CallExpr(exprType, RefExpr(refTy, Some(transform[Expr](referenceObject)), "dropLast", typeParams, true), Seq(LitExpr(KotlinTypes.INT, "1")),
        paramsExpectedTypes.map(transform[Type])))

    //seq.foreach --> seq.forEach
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "foreach", typeParams, true), params, paramsExpectedTypes)
      if TypeUtils.isKotlinList(referenceObject.exprType) =>
      Some(CallExpr(exprType, RefExpr(refTy, Some(transform[Expr](referenceObject)), "forEach", typeParams, true), params.map(transform[Expr]),
        paramsExpectedTypes.map(transform[Type])))

    //     str * i => str.repeat(i)
    case CallExpr(exprType, RefExpr(refTy, Some(left), "*", _, _), Seq(right), paramsExpectedTypes)
      if left.exprType == KotlinTypes.STRING && right.exprType == KotlinTypes.INT =>
      Some(CallExpr(exprType, RefExpr(exprType, Some(transform[Expr](left)), "repeat", Seq.empty, true), Seq(right),
        paramsExpectedTypes.map(transform[Type])))

    //seq(i) --> seq[i]
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "apply", typeParams, true), Seq(i), paramsExpectedTypes)
      if TypeUtils.isKotlinList(referenceObject.exprType) =>
      Some(BracketsExpr(exprType, transform[Expr](referenceObject), transform[Expr](i)))

    //seq1 ++ seq2  --> seq1+ seq2
    case CallExpr(exprType, RefExpr(refTy, Some(left), "++", typeParams, true), Seq(right), paramsExpectedTypes)
      if TypeUtils.isKotlinList(left.exprType) && TypeUtils.isKotlinList(right.exprType) =>
      Some(Exprs.simpleInfix(exprType, "+", transform[Expr](left), transform[Expr](right)))

    // seq.nonEmpty --> seq.isNotEmpty
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "nonEmpty", typeParams, _), _, paramsExpectedTypes)
      if TypeUtils.isKotlinList(referenceObject.exprType) =>
      Some(CallExpr(exprType, RefExpr(refTy, Some(transform[Expr](referenceObject)), "isNotEmpty", typeParams, true), Seq.empty,
        paramsExpectedTypes.map(transform[Type])))


    // seq.size() --> seq.size
    case CallExpr(exprType, RefExpr(refTy, Some(referenceObject), "size", typeParams, _), _, paramsExpectedTypes)
      if TypeUtils.isKotlinList(referenceObject.exprType) =>
      Some(RefExpr(refTy, Some(transform[Expr](referenceObject)), "size", typeParams, true))

    // seqOfOptions.flatten --> seqOfOptions.filterNotNull()
    case CallExpr(callType, RefExpr(refTy, Some(referenceObject@WithType(ListType(NullableType(_)))), "flatten", typeParams, _), _, paramsExpectedTypes) =>
      Some(CallExpr(callType, RefExpr(refTy, Some(transform[Expr](referenceObject)), "filterNotNull", typeParams, true), Seq.empty,
        paramsExpectedTypes.map(transform[Type])))


    //pairs

    //1 -> 2 --> 1 to 2
    case CallExpr(exprType, RefExpr(refTy, Some(left), "->", _, true), Seq(right), paramsExpectedTypes) =>
      Some(Exprs.simpleInfix(exprType, "to", transform[Expr](left), transform[Expr](right)))

    //p._1 --> p.first
    case RefExpr(refTy, Some(left), "_1", _, false) =>
      Some(RefExpr(refTy, Some(transform[Expr](left)), "first", Seq.empty, false))

    //p._2 --> p.second
    case RefExpr(refTy, Some(left), "_1", _, false) =>
      Some(RefExpr(refTy, Some(transform[Expr](left)), "first", Seq.empty, false))

    case RefExpr(refTy, Some(referenceObject), "asInstanceOf", Seq(typeParam), false) =>
      Some(ParenthesesExpr(Exprs.as(transform[Expr](referenceObject), typeParam)))

    case RefExpr(refTy, Some(referenceObject), "isInstanceOf", Seq(typeParam), false) =>
      Some(ParenthesesExpr(Exprs.is(transform[Expr](referenceObject), typeParam)))

    case _ => None
  }

}