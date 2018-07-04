package org.jetbrains.plugins.kotlinConverter

import com.intellij.psi.{PsiClass, PsiCodeBlock, PsiElement, PsiStatement}
import org.jetbrains.plugins.kotlinConverter.ast.Stmt._
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScBlock, ScBlockExpr, ScExpression, ScInfixExpr}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScFunctionDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.{ScImportExpr, ScImportStmt}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScClass
import org.jetbrains.plugins.scala.lang.psi.impl.expr.ScBlockExprImpl
import org.jetbrains.plugins.scala.lang.psi.impl.statements.FakePsiStatement
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult

object ASTConverter {
  def convert[T](psi: PsiElement): T = (psi match {
    case x: ScalaFile =>
      Stmt.FileDef(x.getName, x.getImportStatements.flatMap(_.importExprs).map(convert[Stmt.ImportDef]), x.getClasses.map(convert[Stmt.Def]))
    case x: ScImportExpr =>
      ImportDef(x.reference.map(_.getText).get, x.importedNames)
    case x: ScClass =>
      Stmt.ClassDef(x.name, x.extendsBlock.functions.map(convert[Stmt.Def]))
    case x: ScFunctionDefinition =>
      Stmt.DefnDef(x.name,convert(x.`type`()), Seq.empty, x.body.map(convert[Stmt.Block]).getOrElse(Stmt.EmptyBlock))
    case x: ScBlockExpr =>
      Stmt.MultiBlock(x.statements.map(convert[Expr]))
    case x: ScInfixExpr =>
      Expr.BinExpr(convert(x.`type`()), BinOp(x.operation.getText), convert[Expr](x.left), convert[Expr](x.right))
    case x: ScLiteral =>
      Expr.Lit(convert(x.`type`()), x.getText)
  }).asInstanceOf[T]

  def convert(t: TypeResult) =  Type(t.getOrAny.canonicalText)
}
