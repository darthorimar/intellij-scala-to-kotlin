package org.jetbrains.plugins.kotlinConverter.ast

trait Stmt extends Expr

object Stmt {
  case class If(cond: Expr, trueB: Block, falseB: Block) extends Stmt
  case class For(range: Expr, body: Block) extends Stmt
  case class While(cond: Expr, body: Block) extends Stmt


  trait Block extends Stmt {
    def stmts: Seq[Expr]
    def isSingle: Boolean = stmts.size == 1
    def isEmpty: Boolean = stmts.isEmpty
  }
  case class SingleBlock(stmt: Expr) extends Block {
    override def stmts: Seq[Expr] = Seq(stmt)
  }
  case class MultiBlock(stmts: Seq[Expr]) extends Block

  case object EmptyBlock extends  Block {
    override def stmts: Seq[Expr] = Seq.empty
  }

  trait Def extends Stmt
  case class Defn(attrs: Seq[Attr],
                  t: DefnType,
                  name: String,
                  construct: Option[Construct],
                  supers: Seq[Super],
                  block: Block) extends Def
  case class ValDef(name: String, ty: TypeCont, expr: Expr) extends Def
  case class VarDef(name: String, ty: TypeCont, expr: Expr) extends Def
  case class DefnDef(name: String, ty: TypeCont, args: Seq[DefParam], body: Block) extends Def
  case class ImportDef(ref: String, names: Seq[String]) extends Def

  case class Super(ty: TypeCont, construct: Option[Construct]) extends AST


  trait Top extends Stmt
  case class FileDef(pckg: String, imports: Seq[ImportDef], defns: Seq[Def]) extends Top
}