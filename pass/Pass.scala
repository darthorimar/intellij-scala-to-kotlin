package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.ast.Expr._
import org.jetbrains.plugins.kotlinConverter.ast.Stmt._
import org.jetbrains.plugins.kotlinConverter.ast.{WildcardPattern, _}

trait Pass {
  protected def action(ast: AST): Option[AST]

  private var parentsStack = List.empty[AST]

  protected def parents: List[AST] = parentsStack

  final def pass[T](ast: AST): T = {
    println(" " * parentsStack.size + ast.getClass.getSimpleName)
    parentsStack = ast :: parentsStack
    val res = action(ast).getOrElse(copy(ast)).asInstanceOf[T]
    parentsStack = parentsStack.tail
    res
  }

  private def copy(ast: AST): AST = ast match {
    case ClassDef(name, supers, block) =>
      ClassDef(name, supers.map(pass[Type]), pass[Block](block))

    case TraitDef(name, supers, block) =>
      TraitDef(name, supers.map(pass[Type]), pass[Block](block))

    case ObjDef(name, supers, block) =>
      ObjDef(name, supers.map(pass[Type]), pass[Block](block))

    case ValDef(name, ty, expr) =>
      ValDef(name, pass[Type](ty), pass[Expr](expr))

    case VarDef(name, ty, expr) =>
      VarDef(name, pass[Type](ty), pass[Expr](expr))

    case DefnDef(name, ty, args, body) =>
      DefnDef(name, pass[Type](ty), args.map(pass[DefParam]), pass[Block](body))

    case ImportDef(ref, names) =>
      ImportDef(ref, names)

    case FileDef(pckg, imports, defns) =>
      FileDef(pckg, imports.map(pass[ImportDef]), defns.map(pass[Def]))

    case BinExpr(ty, op, left, right) =>
      BinExpr(pass[Type](ty), op, pass[Expr](left), pass[Expr](right))

    case ParenExpr(inner) =>
      ParenExpr(pass[Expr](inner))

    case Call(ty, ref, typeParams, params) =>
      Call(pass[Type](ty), pass[Expr](ref), typeParams.map(pass[TypeParam]), params.map(pass[Expr]))

    case Lit(ty, name) =>
      Lit(pass[Type](ty), name)

    case UnderSc =>
      UnderSc

    case Ref(ty, name) =>
      Ref(pass[Type](ty), name)

    case Match(expr, clauses) =>
      Match(pass[Expr](expr), clauses.map(pass[CaseClause]))

    case MultiBlock(stmts) =>
      MultiBlock(stmts.map(pass[Expr]))

    case SingleBlock(stmt) =>
      SingleBlock(pass[Expr](stmt))

    case EmptyBlock =>
      EmptyBlock

    case Assign(left, right) =>
      Assign(pass[Expr](left), pass[Expr](right))

    case New(name, args) =>
      New(name, args.map(pass[Expr]))

    case Lambda(params, expr) =>
      Lambda(params.map(pass[DefParam]), pass[Expr](expr))

    case If(cond, trueB, falseB) =>
      If(pass[Expr](cond), pass[Block](trueB), pass[Block](falseB))

    case For(range, body) =>
      For(pass[Expr](range), pass[Block](body))

    case While(cond, body) =>
      While(pass[Expr](cond), pass[Block](body))

    case Type(real, inferenced) =>
      Type(real, inferenced)

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
  }
}

object Pass {
  def applyPasses(ast: AST): AST = {
    new LambdaPass().pass[AST](ast)
  }
}