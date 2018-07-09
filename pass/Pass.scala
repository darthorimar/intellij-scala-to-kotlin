package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.ast._

trait Pass {
  protected def action(ast: AST): Option[AST]

  private var parentsStack = List.empty[AST]

  protected def parents: List[AST] =
    parentsStack.tail

  protected def parent: AST =
    parents.head

  final def pass[T](ast: AST): T = {
//    println(" " * parentsStack.size + ast.getClass.getSimpleName)
    parentsStack = ast :: parentsStack
    val res = action(ast).getOrElse(copy(ast)).asInstanceOf[T]
    parentsStack = parentsStack.tail
    res
  }

  protected def copy(ast: AST): AST = ast match {
    case Defn(attrs, t, name, construct, supers, block) =>
      Defn(attrs.map(pass[Attr]), t, name, construct.map(pass[Construct]), supers.map(pass[Super]), pass[BlockExpr](block))

    case Super(ty, construct) =>
      Super(pass[Type](ty), construct.map(pass[Construct]))
    case EmptyConstruct => EmptyConstruct

    case ParamsConstruct(params) =>
      ParamsConstruct(params.map(pass[ConstructParam]))

    case ConstructParam(parType, mod, name, ty) =>
      ConstructParam(parType, mod, name, pass[Type](ty))

    case ValDef(name, ty, expr) =>
      ValDef(name, pass[Type](ty), pass[Expr](expr))

    case VarDef(name, ty, expr) =>
      VarDef(name, pass[Type](ty), pass[Expr](expr))

    case DefnDef(attrss, name, ty, args, retType, body) =>
      DefnDef(attrss, name, pass[Type](ty), args.map(pass[DefParam]), pass[Type](retType), pass[BlockExpr](body))

    case ImportDef(ref, names) =>
      ImportDef(ref, names)

    case FileDef(pckg, imports, defns) =>
      FileDef(pckg, imports.map(pass[ImportDef]), defns.map(pass[DefExpr]))

    case BinExpr(ty, op, left, right) =>
      BinExpr(pass[Type](ty), op, pass[Expr](left), pass[Expr](right))

    case ParenExpr(inner) =>
      ParenExpr(pass[Expr](inner))

    case CallExpr(ty, ref, typeParams, params) =>
      CallExpr(pass[Type](ty), pass[Expr](ref), typeParams.map(pass[TypeParam]), params.map(pass[Expr]))

    case LitExpr(ty, name) =>
      LitExpr(pass[Type](ty), name)

    case UnderScExpr(ty) =>
      UnderScExpr(ty)

    case InvExpr(ty, obj, ref) =>
      InvExpr(pass[Type](ty), obj.map(pass[Expr]), pass[Expr](ref))

    case RefExpr(ty, name) =>
      RefExpr(pass[Type](ty), name)

    case MatchExpr(ty, expr, clauses) =>
      MatchExpr(pass[Type](ty), pass[Expr](expr), clauses.map(pass[CaseClause]))

    case MultiBlock(stmts) =>
      MultiBlock(stmts.map(pass[Expr]))

    case SingleBlock(stmt) =>
      SingleBlock(pass[Expr](stmt))

    case EmptyBlock =>
      EmptyBlock

    case AssignExpr(left, right) =>
      AssignExpr(pass[Expr](left), pass[Expr](right))

    case NewExpr(ty, name, args) =>
      NewExpr(pass[Type](ty), name, args.map(pass[Expr]))

    case LambdaExpr(ty, params, expr) =>
      LambdaExpr(pass[Type](ty), params.map(pass[DefParam]), pass[Expr](expr))

    case IfExpr(ty, cond, trueB, falseB) =>
      IfExpr(pass[Type](ty), pass[Expr](cond), pass[BlockExpr](trueB), pass[BlockExpr](falseB))

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

    case CaseClause(pattern, expr) =>
      CaseClause(pattern, pass[Expr](expr))

    case TypeParam(ty) =>
      TypeParam(ty)

    case LitPattern(lit) =>
      LitPattern(lit)

    case ConstructorPattern(ref, args) =>
      ConstructorPattern(ref, args.map(pass))

    case TypedPattern(ref, ty) =>
      TypedPattern(ref, pass[Type](ty))

    case ReferencePattern(ref) =>
      ReferencePattern(ref)

    case WildcardPattern =>
      WildcardPattern

    case EmptyAst => EmptyAst
    case x: Keyword => x
  }
}

object Pass {
  def applyPasses(ast: AST): AST = {
    new BasicPass().pass[AST](new TypePass().pass[AST](ast))
  }
}