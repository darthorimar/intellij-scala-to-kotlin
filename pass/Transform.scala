package org.jetbrains.plugins.kotlinConverter.pass

import com.intellij.formatting.BlockEx
import org.jetbrains.plugins.kotlinConverter
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.scopes.{LocalNamer, Renames, ScopedVal}

trait Transform {
  protected def action(ast: AST): Option[AST]

  var collectedImports: List[ImportDef] = Nil
  val renamesVal = new ScopedVal[Renames](Renames(Map.empty))
  val namerVal = new ScopedVal[LocalNamer](new LocalNamer)

  private var parentsStack = List.empty[AST]

  protected def parents: List[AST] =
    parentsStack.tail

  protected def parent: AST =
    parents.head

  def transform[T](ast: AST): T = {
    parentsStack = ast :: parentsStack
    val res = action(ast).getOrElse(copy(ast)).asInstanceOf[T]
    parentsStack = parentsStack.tail
    res
  }

  protected def copy(ast: AST): AST = ast match {
    case x: ErrorAst =>
      x

    case ReturnExpr(label, expr) =>
      ReturnExpr(label, expr.map(transform[Expr]))

    case Defn(attrs, t, name, typeParams, construct, supersBlock, block) =>
      Defn(attrs.map(transform[Attribute]),
        t,
        name,
        typeParams.map(transform[TypeParam]),
        construct.map(transform[Constructor]),
        supersBlock.map(transform[SupersBlock]),
        block.map(transform[BlockExpr]))

    case SupersBlock(constructor, supers) =>
      SupersBlock(constructor.map(transform[SuperConstructor]), supers.map(transform[Type]))

    case SuperConstructor(exprType, exprs) =>
      SuperConstructor(transform[Type](exprType), exprs.map(transform[Expr]))

    case EmptyConstructor => EmptyConstructor

    case ForInExpr(exprType, value, range, body) =>
      ForInExpr(transform[Type](exprType), transform[RefExpr](value), transform[Expr](range), transform[Expr](body))

    case ForGenerator(pattern, expr) =>
      ForGenerator(transform[CasePattern](pattern), transform[Expr](expr))

    case ForGuard(condition) =>
      ForGuard(transform[Expr](condition))

    case ForVal(expr) =>
      ForVal(transform[Expr](expr))

    case ParamsConstructor(params) =>
      ParamsConstructor(params.map(transform[ConstructorParam]))

    case ConstructorParam(parType, mod, name, parameterType) =>
      ConstructorParam(parType, mod, name, transform[Type](parameterType))

    case SimpleValOrVarDef(attributes, isVal, name, valType, expr) =>
      SimpleValOrVarDef(attributes, isVal, name, valType.map(transform[Type]), expr.map(transform[Expr]))

    case LazyValDef(name, exprType, expr) =>
      LazyValDef(name, transform[Type](exprType), transform[Expr](expr))

    case ValOrVarDef(attributes, isVal, patterns, expr) =>
      ValOrVarDef(attributes, isVal, patterns.map(transform[CasePattern]), expr.map(transform[Expr]))

    case DefnDef(attrss, name, typeParams, args, retType, body) =>
      DefnDef(attrss,
        name,
        typeParams.map(transform[TypeParam]),
        args.map(transform[DefParameter]),
        transform[Type](retType),
        body.map(transform[Expr]))

    case ImportDef(ref, names) =>
      ImportDef(ref, names)

    case FileDef(pckg, imports, defns) =>
      val newDefns = defns.map(transform[DefExpr])
      FileDef(pckg, (imports ++ collectedImports).map(transform[ImportDef]), newDefns)

    case BinExpr(exprType, op, left, right) =>
      BinExpr(transform[Type](exprType), op, transform[Expr](left), transform[Expr](right))

    case ParenthesesExpr(inner) =>
      ParenthesesExpr(transform[Expr](inner))

    case TypeExpr(exprType) =>
      TypeExpr(transform[Type](exprType))

    case CallExpr(exprType, ref, params) =>
      CallExpr(transform[Type](exprType),
        transform[Expr](ref),
        params.map(transform[Expr]))

    case WhenExpr(exprType, expr, clauses) =>
      WhenExpr(transform[Type](exprType), expr.map(transform[Expr]), clauses.map(transform[WhenClause]))

    case ElseWhenClause(expr) =>
      ElseWhenClause(transform[Expr](expr))

    case ExprWhenClause(clause, expr) =>
      ExprWhenClause(transform[Expr](clause), transform[Expr](expr))

    case InterpolatedStringExpr(parts, injected) =>
      InterpolatedStringExpr(parts, injected.map(transform[Expr]))

    case LitExpr(exprType, name) =>
      LitExpr(transform[Type](exprType), name)

    case UnderscoreExpr(exprType) =>
      UnderscoreExpr(exprType)

    case RefExpr(exprType, obj, ref, typeParams, isFunc) =>
      RefExpr(transform[Type](exprType), obj.map(transform[Expr]), ref, typeParams.map(transform[TypeParam]), isFunc)

    case MatchExpr(exprType, expr, clauses) =>
      MatchExpr(transform[Type](exprType), transform[Expr](expr), clauses.map(transform[MatchCaseClause]))

    case BlockExpr(exprType, exprs) =>
      BlockExpr(transform[Type](exprType), exprs.map(transform[Expr]))

    case PostfixExpr(exprType, obj, op) =>
      PostfixExpr(exprType, transform[Expr](obj), op)

    case AssignExpr(left, right) =>
      AssignExpr(transform[Expr](left), transform[Expr](right))

    case NewExpr(exprType, name, args) =>
      NewExpr(transform[Type](exprType), name, args.map(transform[Expr]))

    case LambdaExpr(exprType, params, expr, needBraces) =>
      LambdaExpr(transform[Type](exprType), params.map(transform[DefParameter]), transform[Expr](expr), needBraces)

    case IfExpr(exprType, cond, trueB, falseB) =>
      IfExpr(transform[Type](exprType), transform[Expr](cond), transform[Expr](trueB), falseB.map(transform[Expr]))

    case WhileExpr(exprType, cond, body) =>
      WhileExpr(transform[Type](exprType), transform[Expr](cond), transform[BlockExpr](body))

    case ScalaTryExpr(exprType, tryBlock, catchBlock, finallyBlock) =>
      ScalaTryExpr(transform[Type](exprType),
        transform[Expr](tryBlock),
        catchBlock.map(transform[ScalaCatch]),
        finallyBlock.map(transform[Expr]))

    case KotlinTryExpr(exprType, tryBlock, catchCases, finallyBlock) =>
      KotlinTryExpr(transform[Type](exprType),
        transform[Expr](tryBlock),
        catchCases.map(transform[KotlinCatchCase]),
        finallyBlock.map(transform[Expr]))

    case ScalaCatch(cases) =>
      ScalaCatch(cases.map(transform[MatchCaseClause]))

    case KotlinCatchCase(name, valueType, expr) =>
      KotlinCatchCase(name, transform[Type](valueType), transform[Expr](expr))

    case GenerecTypes(des, params) =>
      GenerecTypes(transform[Type](des), params.map(transform[Type]))

    case FunctionType(left, right) =>
      FunctionType(transform[Type](left), transform[Type](right))

    case ProductType(exprTypepes) =>
      ProductType(exprTypepes.map(transform[Type]))

    case SimpleType(name) =>
      SimpleType(name)

    case NullableType(name) =>
      NullableType(transform[Type](name))

    case NoType =>
      NoType

    case DefParameter(exprType, name) =>
      DefParameter(transform[Type](exprType), name)

    case MatchCaseClause(pattern, expr, guard) =>
      MatchCaseClause(pattern, transform[Expr](expr), guard.map(transform[Expr]))

    case TypeParam(exprType) =>
      TypeParam(transform[Type](exprType))

    case LitPattern(lit) =>
      LitPattern(lit)

    case ConstructorPattern(ref, args, label, repr) =>
      ConstructorPattern(ref, args.map(transform[CasePattern]), label, repr)

    case TypedPattern(ref, exprType) =>
      TypedPattern(ref, transform[Type](exprType))

    case ReferencePattern(ref) =>
      ReferencePattern(ref)

    case WildcardPattern =>
      WildcardPattern

    case ThisExpr(exprType) =>
      ThisExpr(transform[Type](exprType))

    case ForExpr(exprType, generators, isYield, body) =>
      ForExpr(transform[Type](exprType), generators.map(transform[ForEnumerator]), isYield, transform[Expr](body))

    case ForGenerator(pattern, expr) =>
      ForGenerator(transform[CasePattern](pattern), transform[Expr](expr))

    case x: Keyword => x
  }
}

object Transform {
  def applyPasses(fileDef: FileDef): FileDef = {
    val passes = Seq(
      new TypeTransform,
      new BasicTransform,
      new CollectionTransform)
    passes.foldLeft(fileDef)((a, p) => p.transform[FileDef](a))
  }
}

