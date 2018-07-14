package org.jetbrains.plugins.kotlinConverter.pass

import com.intellij.formatting.BlockEx
import org.jetbrains.plugins.kotlinConverter
import org.jetbrains.plugins.kotlinConverter.ast._

trait Pass {
  protected def action(ast: AST): Option[AST]

  private var parentsStack = List.empty[AST]

  protected def parents: List[AST] =
    parentsStack.tail

  protected def parent: AST =
    parents.head

  final def pass[T](ast: AST): T = {
    parentsStack = ast :: parentsStack
    val res = action(ast).getOrElse(copy(ast)).asInstanceOf[T]
    parentsStack = parentsStack.tail
    res
  }
  
  protected def copy(ast: AST): AST = ast match {
    case ReturnExpr(label, expr) =>
      ReturnExpr(label, expr.map(pass[Expr]))

    case Defn(attrs, t, name, construct, supers, block) =>
      Defn(attrs.map(pass[Attr]), t, name, construct.map(pass[Construct]), supers.map(pass[Super]), block.map(pass[BlockExpr]))

    case Super(ty, construct) =>
      Super(pass[Type](ty), construct.map(pass[Construct]))
    case EmptyConstruct => EmptyConstruct

    case ParamsConstruct(params) =>
      ParamsConstruct(params.map(pass[ConstructParam]))

    case ConstructParam(parType, mod, name, ty) =>
      ConstructParam(parType, mod, name, pass[Type](ty))

    case ValDef(destructors, expr) =>
      ValDef(destructors.map(pass[MatchCasePattern]), pass[Expr](expr))

    case LazyValDef(name, ty, expr) =>
      LazyValDef(name, pass[Type](ty), pass[Expr](expr))

    case VarDef(name, ty, expr) =>
      VarDef(name, pass[Type](ty), pass[Expr](expr))

    case DefnDef(attrss, name, ty, args, retType, body) =>
      DefnDef(attrss, name, pass[Type](ty), args.map(pass[DefParam]), pass[Type](retType), body.map(pass[Expr]))

    case ImportDef(ref, names) =>
      ImportDef(ref, names)

    case FileDef(pckg, imports, defns) =>
      FileDef(pckg, imports.map(pass[ImportDef]), defns.map(pass[DefExpr]))

    case BinExpr(ty, op, left, right) =>
      BinExpr(pass[Type](ty), op, pass[Expr](left), pass[Expr](right))

    case ParenExpr(inner) =>
      ParenExpr(pass[Expr](inner))

    case TypeExpr(ty) =>
      TypeExpr(pass[Type](ty))

    case CallExpr(ty, ref, params) =>
      CallExpr(pass[Type](ty),
        pass[Expr](ref),
        params.map(pass[Expr]))

    case WhenExpr(ty, expr, clauses) =>
      WhenExpr(pass[Type](ty), expr.map(pass[Expr]), clauses.map(pass[WhenClause]))

    case ElseWhenClause(expr) =>
      ElseWhenClause(pass[Expr](expr))

    case ExprWhenClause(clause, expr) =>
      ExprWhenClause(pass[Expr](clause), pass[Expr](expr))


    case LitExpr(ty, name) =>
      LitExpr(pass[Type](ty), name)

    case UnderScExpr(ty) =>
      UnderScExpr(ty)

    case RefExpr(ty, obj, ref, typeParams, isFunc) =>
      RefExpr(pass[Type](ty), obj.map(pass[Expr]), ref, typeParams.map(pass[TypeParam]), isFunc)

    case MatchExpr(ty, expr, clauses) =>
      MatchExpr(pass[Type](ty), pass[Expr](expr), clauses.map(pass[MatchCaseClause]))

    case BlockExpr(exprs) =>
      BlockExpr(exprs.map(pass[Expr]))

    case PostExpr(ty, obj, op) =>
      PostExpr(ty, pass[Expr](obj), op)

    case AssignExpr(left, right) =>
      AssignExpr(pass[Expr](left), pass[Expr](right))

    case NewExpr(ty, name, args) =>
      NewExpr(pass[Type](ty), name, args.map(pass[Expr]))

    case LambdaExpr(ty, params, expr, needBraces) =>
      LambdaExpr(pass[Type](ty), params.map(pass[DefParam]), pass[Expr](expr), needBraces)

    case IfExpr(ty, cond, trueB, falseB) =>
      IfExpr(pass[Type](ty), pass[Expr](cond), pass[Expr](trueB), falseB.map(pass[Expr]))

    case ForExpr(ty, range, body) =>
      ForExpr(pass[Type](ty), pass[Expr](range), pass[BlockExpr](body))

    case WhileExpr(ty, cond, body) =>
      WhileExpr(pass[Type](ty), pass[Expr](cond), pass[BlockExpr](body))

    case PType(des, params) =>
      PType(pass[Type](des), params.map(pass[Type]))

    case FuncType(left, right) =>
      FuncType(pass[Type](left), pass[Type](right))

    case ProdType(types) =>
      ProdType(types.map(pass[Type]))

    case SimpleType(name) =>
      SimpleType(name)

    case NullableType(name) =>
      NullableType(pass[Type](name))

    case NoType =>
      NoType

    case DefParam(ty, name) =>
      DefParam(pass[Type](ty), name)

    case MatchCaseClause(pattern, expr, guard) =>
      MatchCaseClause(pattern, pass[Expr](expr), guard.map(pass[Expr]))

    case TypeParam(ty) =>
      TypeParam(ty)

    case LitPatternMatch(lit) =>
      LitPatternMatch(lit)

    case ConstructorPatternMatch(ref, args, label,  repr) =>
      ConstructorPatternMatch(ref, args.map(pass[MatchCasePattern]), label, repr)

    case TypedPatternMatch(ref, ty) =>
      TypedPatternMatch(ref, pass[Type](ty))

    case ReferencePatternMatch(ref) =>
      ReferencePatternMatch(ref)

    case WildcardPatternMatch =>
      WildcardPatternMatch

    case x: Keyword => x
  }
}

object Pass {
  def applyPasses(ast: AST): AST = {
    val passes = Seq(
      new TypePass,
      new BasicPass,
      new CollectionPass)
    passes.foldLeft(ast)((a, p) => p.pass[AST](a))
  }
}

