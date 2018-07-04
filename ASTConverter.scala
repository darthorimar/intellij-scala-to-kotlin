package org.jetbrains.plugins.kotlinConverter

import com.intellij.psi.{PsiClass, PsiCodeBlock, PsiElement, PsiStatement}
import org.jetbrains.plugins.kotlinConverter.ast.Stmt._
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScCaseClause, ScLiteralPattern, ScPattern, ScWildcardPattern}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScFunctionDeclaration, ScFunctionDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.{ScImportExpr, ScImportStmt}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScClass
import org.jetbrains.plugins.scala.lang.psi.impl.expr.ScBlockExprImpl
import org.jetbrains.plugins.scala.lang.psi.impl.statements.FakePsiStatement
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult

import scala.collection.immutable

object ASTConverter {
  private def genDefinitions(file: ScalaFile): Seq[PsiElement] = {
    println(file.typeDefinitions.mkString(", "))
    val functionDefns =
      file.findChildrenByType(ScalaElementTypes.FUNCTION_DEFINITION)
    val functionDecls =
      file.findChildrenByType(ScalaElementTypes.FUNCTION_DECLARATION)
    functionDefns ++ functionDecls ++ file.getClasses
  }
  private def genFunctionBody(fun: ScFunction) = fun match {
    case x: ScFunctionDefinition =>
      x.body.map(gen[Stmt.Block]).getOrElse(Stmt.EmptyBlock)
    case _: ScFunctionDeclaration =>
      Stmt.EmptyBlock
  }
  def getTypeArgs(genCall: ScGenericCall) = {
    genCall.typeArgs.
      map(_.typeArgs)
      .toSeq
      .flatten
      .map(z => TypeParam(genType(z.`type`())))
  }
  def genType(t: TypeResult) = Type(t.getOrAny.canonicalText)

  def gen[T](psi: PsiElement): T = (psi match {
    case x: ScalaFile =>
      Stmt.FileDef(
        x.getName,
        x.getImportStatements.flatMap(_.importExprs).map(gen[Stmt.ImportDef]),
        genDefinitions(x).map(gen[Stmt.Def]))
    case x: ScImportExpr =>
      ImportDef(x.reference.map(_.getText).get, x.importedNames)
    case x: ScClass =>
      Stmt.ClassDef(x.name, x.extendsBlock.functions.map(gen[Stmt.Def]))
    case x: ScFunction =>
      Stmt.DefnDef(
        x.name,
        genType(x.returnType),
        x.parameters.map(gen[DefParam]),
        genFunctionBody(x))
    case x: ScParameter =>
      DefParam(genType(x.`type`()), x.name)
    case x: ScBlock =>
      Stmt.MultiBlock(x.statements.map(gen[Expr]))
    case x: ScInfixExpr =>
      Expr.BinExpr(genType(x.`type`()), BinOp(x.operation.getText), gen[Expr](x.left), gen[Expr](x.right))
    case x: ScLiteral =>
      Expr.Lit(genType(x.`type`()), x.getText)
    case x: ScParenthesisedExpr =>
      Expr.ParenExpr(gen[Expr](x.innerElement.get))
    case x: ScReferenceExpression =>
      Expr.Ref(genType(x.`type`()), x.getText)
    case x: ScMethodCall =>
      Expr.Call(
        genType(x.`type`()),
        x.getInvokedExpr match {
          case y: ScGenericCall =>
            gen[Expr](y.referencedExpr)
          case y => gen[Expr](y)
        },
        x.getInvokedExpr match {
          case y: ScGenericCall => getTypeArgs(y)
          case _ => Seq.empty
        },
        x.args.exprs.map(gen[Expr]))
    case x: ScGenericCall =>
      Expr.Call(genType(x.`type`()),
        gen[Expr](x.referencedExpr),
        getTypeArgs(x),
        Seq.empty
      )
    case x: ScMatchStmt =>
      Expr.Match(gen[Expr](x.expr.get), x.caseClauses.map(gen[CaseClause]))
    case x: ScCaseClause =>
      CaseClause(gen[CasePattern](x.pattern.get), gen[Expr](x.expr.get))
    case x: ScLiteralPattern =>
      LitPattern(gen[Expr.Lit](x.getLiteral))
    case x: ScWildcardPattern =>
      WildcardPattern
  }).asInstanceOf[T]
}
