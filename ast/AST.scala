package org.jetbrains.plugins.kotlinConverter.ast

trait AST

case class DefParam(ty: Type, name: String) extends AST
case class CaseClause(pattern: CasePattern, expr: Expr) extends AST


trait CasePattern extends AST
case class LitPattern(lit: Expr.Lit) extends CasePattern
case class ConstructorPattern(ref: String, args: Seq[CasePattern])  extends CasePattern
case class TypedPattern(ref: String, ty: Type) extends CasePattern
case class ReferencePattern(ref: String) extends CasePattern
case object WildcardPattern extends CasePattern

