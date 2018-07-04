package org.jetbrains.plugins.kotlinConverter.ast

trait Expr extends AST

object Expr {
  case class BinExpr(ty: Type, op: BinOp, left: Expr, right: Expr) extends Expr
  case class Call(ty: Type, obj: Expr, method: String, params: Seq[Expr]) extends Expr
  case class Lit(ty: Type, name: String) extends Expr
}


