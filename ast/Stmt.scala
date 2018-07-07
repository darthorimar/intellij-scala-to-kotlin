package org.jetbrains.plugins.kotlinConverter.ast

trait Stmt extends Expr

object Stmt {
  case class If(cond: Expr, trueB: Block, falseB: Block) extends Stmt
  case class For(range: Expr, body: Block) extends Stmt
  case class While(cond: Expr, body: Block) extends Stmt


  trait Block extends Stmt {
    def stmts: Seq[Expr]
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
                  supers: Seq[Type],
                  block: Block) extends Def
  case class ValDef(name: String, ty: Type, expr: Expr) extends Def
  case class VarDef(name: String, ty: Type, expr: Expr) extends Def
  case class DefnDef(name: String, ty: Type, args: Seq[DefParam], body: Block) extends Def
  case class ImportDef(ref: String, names: Seq[String]) extends Def



  trait Top extends Stmt
  case class FileDef(pckg: String, imports: Seq[ImportDef], defns: Seq[Def]) extends Top
}