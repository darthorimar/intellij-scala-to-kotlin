package org.jetbrains.plugins.kotlinConverter.pass

import com.intellij.formatting.BlockEx
import org.jetbrains.plugins.kotlinConverter
import org.jetbrains.plugins.kotlinConverter.ast._

trait Transform {
  protected def action(ast: AST): Option[AST]
  var collectedImports: List[ImportDef] = Nil

  private var parentsStack = List.empty[AST]

  protected def parents: List[AST] =
    parentsStack.tail

  protected def parent: AST =
    parents.head

  def pass[T](ast: AST): T = {
    parentsStack = ast :: parentsStack
    val res = action(ast).getOrElse(copy(ast)).asInstanceOf[T]
    parentsStack = parentsStack.tail
    res
  }

  protected def copy(ast: AST): AST = ast match {
    case ReturnExpr(label, expr) =>
      ReturnExpr(label, expr.map(pass[Expr]))

    case Defn(attrs, t, name, typeParams, construct, supersBlock, block) =>
      Defn(attrs.map(pass[Attribute]),
        t,
        name,
        typeParams.map(pass[TypeParam]),
        construct.map(pass[Constructor]),
        supersBlock.map(pass[SupersBlock]),
        block.map(pass[BlockExpr]))

    case SupersBlock(constructor, supers) =>
      SupersBlock(constructor.map(pass[SuperConstructor]), supers.map(pass[Type]))

    case SuperConstructor(exprType, exprs) =>
      SuperConstructor(pass[Type](exprType), exprs.map(pass[Expr]))

    case EmptyConstructor => EmptyConstructor

    case ForInExpr(exprType, value, range, body) =>
      ForInExpr(pass[Type](exprType), pass[RefExpr](value), pass[Expr](range), pass[Expr](body))

    case ForGenerator(pattern, expr) =>
      ForGenerator(pass[CasePattern](pattern), pass[Expr](expr))

    case ForGuard(condition) =>
      ForGuard(pass[Expr](condition))

    case ForVal(expr) =>
      ForVal(pass[Expr](expr))

    case ParamsConstructor(params) =>
      ParamsConstructor(params.map(pass[ConstructorParam]))

    case ConstructorParam(parType, mod, name, parameterType) =>
      ConstructorParam(parType, mod, name, pass[Type](parameterType))

    case SimpleValOrVarDef(attributes, isVal, name, valType, expr) =>
      SimpleValOrVarDef(attributes, isVal, name, valType.map(pass[Type]), expr.map(pass[Expr]))

    case LazyValDef(name, exprType, expr) =>
      LazyValDef(name, pass[Type](exprType), pass[Expr](expr))

    case ValOrVarDef(attributes, isVal, patterns, expr) =>
      ValOrVarDef(attributes, isVal, patterns.map(pass[CasePattern]), expr.map(pass[Expr]))

    case DefnDef(attrss, name, typeParams, args, retType, body) =>
      DefnDef(attrss,
        name,
        typeParams.map(pass[TypeParam]),
        args.map(pass[DefParameter]),
        pass[Type](retType),
        body.map(pass[Expr]))

    case ImportDef(ref, names) =>
      ImportDef(ref, names)

    case FileDef(pckg, imports, defns) =>
      val newDefns = defns.map(pass[DefExpr])
      FileDef(pckg, (imports ++ collectedImports).map(pass[ImportDef]), newDefns)

    case BinExpr(exprType, op, left, right) =>
      BinExpr(pass[Type](exprType), op, pass[Expr](left), pass[Expr](right))

    case ParenthesesExpr(inner) =>
      ParenthesesExpr(pass[Expr](inner))

    case TypeExpr(exprType) =>
      TypeExpr(pass[Type](exprType))

    case CallExpr(exprType, ref, params) =>
      CallExpr(pass[Type](exprType),
        pass[Expr](ref),
        params.map(pass[Expr]))

    case WhenExpr(exprType, expr, clauses) =>
      WhenExpr(pass[Type](exprType), expr.map(pass[Expr]), clauses.map(pass[WhenClause]))

    case ElseWhenClause(expr) =>
      ElseWhenClause(pass[Expr](expr))

    case ExprWhenClause(clause, expr) =>
      ExprWhenClause(pass[Expr](clause), pass[Expr](expr))

    case InterpolatedStringExpr(parts, injected) =>
      InterpolatedStringExpr(parts, injected.map(pass[Expr]))

    case LitExpr(exprType, name) =>
      LitExpr(pass[Type](exprType), name)

    case UnderscoreExpr(exprType) =>
      UnderscoreExpr(exprType)

    case RefExpr(exprType, obj, ref, typeParams, isFunc) =>
      RefExpr(pass[Type](exprType), obj.map(pass[Expr]), ref, typeParams.map(pass[TypeParam]), isFunc)

    case MatchExpr(exprType, expr, clauses) =>
      MatchExpr(pass[Type](exprType), pass[Expr](expr), clauses.map(pass[MatchCaseClause]))

    case BlockExpr(exprType, exprs) =>
      BlockExpr(pass[Type](exprType), exprs.map(pass[Expr]))

    case PostfixExpr(exprType, obj, op) =>
      PostfixExpr(exprType, pass[Expr](obj), op)

    case AssignExpr(left, right) =>
      AssignExpr(pass[Expr](left), pass[Expr](right))

    case NewExpr(exprType, name, args) =>
      NewExpr(pass[Type](exprType), name, args.map(pass[Expr]))

    case LambdaExpr(exprType, params, expr, needBraces) =>
      LambdaExpr(pass[Type](exprType), params.map(pass[DefParameter]), pass[Expr](expr), needBraces)

    case IfExpr(exprType, cond, trueB, falseB) =>
      IfExpr(pass[Type](exprType), pass[Expr](cond), pass[Expr](trueB), falseB.map(pass[Expr]))

    case WhileExpr(exprType, cond, body) =>
      WhileExpr(pass[Type](exprType), pass[Expr](cond), pass[BlockExpr](body))

    case TryExpr(tryBlock, finallyBlock) =>
      TryExpr(pass[Expr](tryBlock), finallyBlock.map(pass[Expr]))

    case GenerecTypes(des, params) =>
      GenerecTypes(pass[Type](des), params.map(pass[Type]))

    case FunctionType(left, right) =>
      FunctionType(pass[Type](left), pass[Type](right))

    case ProductType(exprTypepes) =>
      ProductType(exprTypepes.map(pass[Type]))

    case SimpleType(name) =>
      SimpleType(name)

    case NullableType(name) =>
      NullableType(pass[Type](name))

    case NoType =>
      NoType

    case DefParameter(exprType, name) =>
      DefParameter(pass[Type](exprType), name)

    case MatchCaseClause(pattern, expr, guard) =>
      MatchCaseClause(pattern, pass[Expr](expr), guard.map(pass[Expr]))

    case TypeParam(exprType) =>
      TypeParam(pass[Type](exprType))

    case LitPattern(lit) =>
      LitPattern(lit)

    case ConstructorPattern(ref, args, label, repr) =>
      ConstructorPattern(ref, args.map(pass[CasePattern]), label, repr)

    case TypedPattern(ref, exprType) =>
      TypedPattern(ref, pass[Type](exprType))

    case ReferencePattern(ref) =>
      ReferencePattern(ref)

    case WildcardPattern =>
      WildcardPattern

    case ThisExpr(exprType) =>
      ThisExpr(pass[Type](exprType))

    case ForExpr(exprType, generators, isYield,  body) =>
      ForExpr(pass[Type](exprType), generators.map(pass[ForEnumerator]), isYield, pass[Expr](body))

    case ForGenerator(pattern, expr) =>
      ForGenerator(pass[CasePattern](pattern), pass[Expr](expr))

    case x: Keyword => x
  }
}

object Transform {
  def applyPasses(fileDef: FileDef): FileDef = {
    val passes = Seq(
      new TypeTransform,
      new BasicTransform,
      new CollectionTransform)
    passes.foldLeft(fileDef)((a, p) => p.pass[FileDef](a))
  }
}

