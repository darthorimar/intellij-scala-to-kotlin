package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter
import org.jetbrains.plugins.kotlinConverter.Collector
import org.jetbrains.plugins.kotlinConverter.ast.{PostfixExpr, _}
import org.jetbrains.plugins.kotlinConverter.scopes.{LocalNamer, Renamer, ScopedVal}

trait Transform extends Collector {
  protected def action(ast: AST): Option[AST]

  val renamerVal = new ScopedVal[Renamer](Renamer(Map.empty))
  val namerVal = new ScopedVal[LocalNamer](new LocalNamer)

  var context: File = null
  var imports: Set[Import] = Set.empty

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

    case Defn(attrs, t, name, typeParams, construct, supersBlock, block, companionDefn) =>
      Defn(attrs.map(transform[Attribute]),
        t,
        name,
        typeParams.map(transform[TypeParam]),
        construct.map(transform[Constructor]),
        supersBlock.map(transform[SupersBlock]),
        block.map(transform[BlockExpr]),
        companionDefn.map(transform[CompanionModule]))

    case ClassCompanion(companion) =>
      ClassCompanion(transform[Defn](companion))

    case ObjectCompanion => ObjectCompanion

    case SupersBlock(constructor, supers) =>
      SupersBlock(constructor.map(transform[SuperConstructor]), supers.map(transform[Type]))

    case SuperConstructor(exprType, exprs, needBrackets) =>
      SuperConstructor(transform[Type](exprType), exprs.map(transform[Expr]), needBrackets)

    case EmptyConstructor => EmptyConstructor

    case ThrowExpr(exprType, expr) =>
      ThrowExpr(transform[Type](exprType), transform[Expr](expr))

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

    case Import(ref) =>
      Import(ref)

    case x@File(pckg, fileImports, defns, neededDefinitions) =>
      context = x
      val newDefns = defns.map(transform[DefExpr])
      File(pckg, (imports ++ fileImports).map(transform[Import]), newDefns, neededDefinitions ++ collectedDefinitions)

    case InfixExpr(exprType, op, left, right, isLeftAssoc) =>
      InfixExpr(transform[Type](exprType),
        transform[RefExpr](op),
        transform[Expr](left),
        transform[Expr](right),
        isLeftAssoc)

    case ParenthesesExpr(inner) =>
      ParenthesesExpr(transform[Expr](inner))

    case TypeExpr(exprType) =>
      TypeExpr(transform[Type](exprType))

    case CallExpr(exprType, ref, arguments, paramsExpectedTypes) =>
      CallExpr(transform[Type](exprType),
        transform[Expr](ref),
        arguments.map(transform[Expr]),
        paramsExpectedTypes.map(transform[CallParameterInfo]))

    case CallParameterInfo(expectedType, isCallByName) =>
      CallParameterInfo(transform[Type](expectedType), isCallByName)

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
      RefExpr(transform[Type](exprType), obj.map(transform[Expr]), ref, typeParams.map(transform[Type]), isFunc)

    case MatchExpr(exprType, expr, clauses) =>
      MatchExpr(transform[Type](exprType), transform[Expr](expr), clauses.map(transform[MatchCaseClause]))

    case BlockExpr(exprType, exprs) =>
      BlockExpr(transform[Type](exprType), exprs.map(transform[Expr]))

    case PostfixExpr(exprType, obj, op) =>
      PostfixExpr(exprType, transform[Expr](obj), op)

    case PrefixExpr(exprType, obj, op) =>
      PrefixExpr(exprType, transform[Expr](obj), op)

    case AssignExpr(left, right) =>
      AssignExpr(transform[Expr](left), transform[Expr](right))

    case NewExpr(exprType, instanceType, args) =>
      NewExpr(transform[Type](exprType), transform[Type](instanceType), args.map(transform[Expr]))

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

    case GenericType(des, params) =>
      GenericType(transform[Type](des), params.map(transform[Type]))

    case FunctionType(left, right) =>
      FunctionType(transform[Type](left), transform[Type](right))

    case ProductType(exprTypepes) =>
      ProductType(exprTypepes.map(transform[Type]))

    case ScalaStdType(name) =>
      ScalaStdType(name)

    case SimpleType(name) =>
      SimpleType(name)

    case ClassType(name) =>
      ClassType(name)

    case NullableType(name) =>
      NullableType(transform[Type](name))

    case TypeParamType(name) =>
      TypeParamType(name)

    case KotlinCollectionType(name) =>
      KotlinCollectionType(name)

    case JavaType(name) =>
      JavaType(name)

    case ScalaCollectionType(name) =>
      ScalaCollectionType(name)

    case NoType =>
      NoType

    case DefParameter(exprType, name, isVarArg, isCallByName) =>
      DefParameter(transform[Type](exprType), name, isVarArg, isCallByName)

    case MatchCaseClause(pattern, expr, guard) =>
      MatchCaseClause(transform[CasePattern](pattern), transform[Expr](expr), guard.map(transform[Expr]))

    case TypeParam(name) =>
      TypeParam(name)

    case LitPattern(lit) =>
      LitPattern(lit)

    case TuplePattern(parts) =>
      TuplePattern(parts.map(transform[CasePattern]))

    case ConstructorPattern(ref, args, label, repr) =>
      ConstructorPattern(transform[ConstructorRef](ref), args.map(transform[CasePattern]), label, repr)

    case x@CaseClassConstructorRef(name) =>
      x

    case UnapplyCallConstuctorRef(objectName, unapplyReturnType) =>
      UnapplyCallConstuctorRef(objectName, transform[Type](unapplyReturnType))

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

    case BracketsExpr(exprType, expr, inBrackets) =>
      BracketsExpr(transform[Type](exprType), transform[Expr](expr), transform[Expr](inBrackets))

    case EmptyDefExpr => EmptyDefExpr

    case x: Keyword => x
  }
}

object Transform {
  def applyPasses(fileDef: File): File = {
    val passes = Seq(
      new TypeTransform,
      new BasicTransform,
      new CollectionTransform,
      new TypeTransform, //todo get rid of second call
      new RefCollector)
    passes.foldLeft(fileDef)((a, p) => p.transform[File](a)) //todo rename
  }
}

