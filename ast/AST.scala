package org.jetbrains.plugins.kotlinConverter.ast

import org.jetbrains.plugins.kotlinConverter.ast.Stmt.{Block, Top}

trait AST

case object EmptyAst extends AST with Expr with Stmt with Top with Attr with CasePattern with Block {
  override def stmts: Seq[Expr] = Seq.empty
}

case class DefParam(ty: Type, name: String) extends AST
case class CaseClause(pattern: CasePattern, expr: Expr) extends AST


trait CasePattern extends AST
case class LitPattern(lit: Expr.Lit) extends CasePattern
case class ConstructorPattern(ref: String, args: Seq[CasePattern])  extends CasePattern
case class TypedPattern(ref: String, ty: Type) extends CasePattern
case class ReferencePattern(ref: String) extends CasePattern
case object WildcardPattern extends CasePattern

sealed trait Attr extends AST
case object CaseAttr extends Attr


trait Construct extends AST
case class ParamsConstruct(params: Seq[ConstructParam]) extends Construct
case object EmptyConstruct extends Construct

case class ConstructParam(parType: ParamType, mod: ParamModifier, name: String, ty: Type) extends AST

trait ParamModifier extends AST
case object PrivModifier extends ParamModifier
case object PublModifier extends ParamModifier
case object NoModifier extends ParamModifier

trait ParamType extends AST
case object ValType extends ParamType
case object VarType extends ParamType
case object NoType extends ParamType


