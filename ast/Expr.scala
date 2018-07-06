package org.jetbrains.plugins.kotlinConverter.ast

import org.jetbrains.plugins.kotlinConverter.ast.Stmt.Block

trait Expr extends AST

object Expr {
  case class BinExpr(ty: Type, op: BinOp, left: Expr, right: Expr) extends Expr
  case class ParenExpr(inner: Expr) extends Expr
  case class Call(ty: Type, ref: Expr, typeParams: Seq[TypeParam], params: Seq[Expr]) extends Expr
  case class Lit(ty: Type, name: String) extends Expr
  case object UnderSc extends Expr
  case class Ref(ty: Type, name: String) extends Expr
  case class Match(expr: Expr, clauses: Seq[CaseClause]) extends Expr
  case class Assign(left: Expr, right: Expr) extends Expr
  case class New(name: String, args: Seq[Expr]) extends Expr
  case class Lambda(params: Seq[DefParam], expr: Expr) extends Expr
  case class Throw(expr: Expr) extends Expr

}


