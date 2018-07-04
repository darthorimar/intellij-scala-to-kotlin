package org.jetbrains.plugins.kotlinConverter.ast

import org.jetbrains.plugins.kotlinConverter.ast.Stmt.Block

trait AST

case class DefParam(ty: Type, name: String) extends AST
case class CaseClause(pattern: CasePattern, expr: Expr)


trait CasePattern
case class LitPattern(lit: Expr.Lit) extends CasePattern
case object WildcardPattern extends CasePattern