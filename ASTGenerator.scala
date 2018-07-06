package org.jetbrains.plugins.kotlinConverter

import com.intellij.psi.{PsiClass, PsiCodeBlock, PsiElement, PsiStatement}
import org.jetbrains.plugins.kotlinConverter.ast.Stmt._
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScLiteral, ScPrimaryConstructor, ScReferenceElement, ScStableCodeReferenceElement}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns._
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScClassParameter, ScParameter}
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.{ScImportExpr, ScImportStmt}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScObject, ScTrait, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.expr.{ScBlockExprImpl, ScNewTemplateDefinitionImpl}
import org.jetbrains.plugins.scala.lang.psi.impl.statements.FakePsiStatement
import org.jetbrains.plugins.scala.lang.psi.light.PsiClassWrapper
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult

import scala.collection.immutable

object ASTGenerator {
  private def genDefinitions(file: ScalaFile): Seq[PsiElement] = {
    println(file.typeDefinitions.mkString(", "))
    val functionDefns =
      file.findChildrenByType(ScalaElementTypes.FUNCTION_DEFINITION)
    val functionDecls =
      file.findChildrenByType(ScalaElementTypes.FUNCTION_DECLARATION)
    functionDefns ++ functionDecls ++ file.getClasses
  }

  private def genFunctionBody(fun: ScFunction): Block = fun match {
    case x: ScFunctionDefinition =>
      x.body.map(gen[Stmt.Block]).getOrElse(Stmt.EmptyBlock)
    case _: ScFunctionDeclaration =>
      Stmt.EmptyBlock
  }

  private def multiOrEmptyBlock(defs: Seq[Def]): Block =
    if (defs.isEmpty) EmptyBlock
    else MultiBlock(defs)

  def genTypeArgs(genCall: ScGenericCall): Seq[TypeParam] = {
    genCall.typeArgs.
      map(_.typeArgs)
      .toSeq
      .flatten
      .map(z => TypeParam(z.getText))
  }

  def genType(real: Option[ScTypeElement], inf: TypeResult) =
    Type(real.map(_.getText), inf.toOption.map(_.canonicalText))

  def genType(real: Option[ScTypeElement], inf: Option[ScType]) =
    Type(real.map(_.getText), inf.map(_.canonicalText))

  def genAttrs(x: ScTypeDefinition): Seq[Attr] = {
    def attr(p: Boolean, a: Attr) =
      if (p) Some(a) else None

    attr(x.isCase, CaseAttr)
      .toSeq
  }

  def gen[T](psi: PsiElement): T = (psi match {
    case x: ScalaFile =>
      Stmt.FileDef(
        x.getPackageName,
        x.importStatementsInHeader.flatMap(_.importExprs).map(gen[Stmt.ImportDef]),
        genDefinitions(x)
          .filter {
            case _: PsiClassWrapper => false
            case y: ScObject if y.isSyntheticObject => false
            case _ => true
          }
          .map(gen[Stmt.Def]))
    case x: ScImportExpr =>
      ImportDef(x.reference.map(_.getText).get, x.importedNames)
    case x: ScClass =>
      Stmt.ClassDef(
        genAttrs(x),
        x.name,
        x.constructor.map(gen[Construct]).getOrElse(EmptyConstruct),
        Seq.empty,
        multiOrEmptyBlock(
          x.extendsBlock.members.map(gen[Stmt.Def])))
    case x: PsiClassWrapper =>
      gen[Def](x.definition)
    case x: ScTrait =>
      Stmt.TraitDef(
        x.name,
        Seq.empty,
        multiOrEmptyBlock(
          x.extendsBlock.members.map(gen[Stmt.Def])))

    case x: ScObject =>
      x.fakeCompanionClass
      Stmt.ObjDef(
        x.name,
        Seq.empty,
        multiOrEmptyBlock(
          x.extendsBlock.members.map(gen[Stmt.Def])))

    case x: ScFunction =>
      Stmt.DefnDef(
        x.name,
        genType(x.returnTypeElement, x.`type`()),
        x.parameters.map(gen[DefParam]),
        genFunctionBody(x))

    case x: ScBlock =>
      if (x.hasRBrace || x.statements.size > 1)
        Stmt.MultiBlock(x.statements.map(gen[Expr]))
      else if (x.statements.isEmpty)
        Stmt.EmptyBlock
      else
        Stmt.SingleBlock(gen[Expr](x.statements.head))

    case x: ScInfixExpr =>
      Expr.BinExpr(genType(None, x.`type`()), BinOp(x.operation.getText), gen[Expr](x.left), gen[Expr](x.right))
    case x: ScLiteral =>
      Expr.Lit(genType(None, x.`type`()), x.getText)
    case x: ScUnderscoreSection =>
      Expr.UnderSc
    case x: ScParenthesisedExpr =>
      Expr.ParenExpr(gen[Expr](x.innerElement.get))
    case x: ScReferenceExpression =>
      Expr.Ref(genType(None, x.`type`()), x.getText)
    case x: ScMethodCall =>
      Expr.Call(
        genType(None, x.`type`()),
        x.getInvokedExpr match {
          case y: ScGenericCall =>
            gen[Expr](y.referencedExpr)
          case y => gen[Expr](y)
        },
        x.getInvokedExpr match {
          case y: ScGenericCall => genTypeArgs(y)
          case _ => Seq.empty
        },
        x.args.exprs.map(gen[Expr]))
    case x: ScGenericCall =>
      Expr.Call(genType(None, x.`type`()),
        gen[Expr](x.referencedExpr),
        genTypeArgs(x),
        Seq.empty
      )
    //    case x: ScThrowStmt =>
    //      Expr.Throw(gen[Expr](x.body.get))
    case x: ScMatchStmt =>
      Expr.Match(gen[Expr](x.expr.get), x.caseClauses.map(gen[CaseClause]))
    case x: ScFunctionExpr =>
      Expr.Lambda(x.parameters.map(gen[DefParam]), gen[Expr](x.result.get))
    case x: ScCaseClause =>
      CaseClause(gen[CasePattern](x.pattern.get), gen[Expr](x.expr.get))
    case x: ScLiteralPattern =>
      LitPattern(gen[Expr.Lit](x.getLiteral))
    case x: ScConstructorPattern =>
      ConstructorPattern(x.ref.qualName, x.args.patterns.map(gen[CasePattern]))
    case x: ScTypedPattern =>
      TypedPattern(x.name, genType(x.typePattern.map(_.typeElement), None))
    case x: ScReferencePattern =>
      ReferencePattern(x.name)
    case x: ScReferenceElement =>
      ReferencePattern(x.refName)
    case x: ScStableReferenceElementPattern =>
      ReferencePattern(x.getReferenceExpression.get.refName)
    case _: ScWildcardPattern =>
      WildcardPattern
    case x: ScPatternDefinition =>
      Stmt.ValDef(
        x.bindings.head.name,
        genType(x.typeElement, x.`type`()),
        gen[Expr](x.expr.get))
    case x: ScVariableDefinition =>
      Stmt.VarDef(
        x.bindings.head.name,
        genType(x.typeElement, x.`type`()),
        gen[Expr](x.expr.get))
    case x: ScAssignStmt =>
      Expr.Assign(gen[Expr](x.getLExpression), gen[Expr](x.getRExpression.get))
    case x: ScNewTemplateDefinitionImpl =>
      Expr.New(
        x.constructor.get.typeElement.getText,
        x.constructor.get.args.toSeq.flatMap(_.exprs).map(gen[Expr]))
    case x: ScPrimaryConstructor =>
      ParamsConstruct(x.parameters.map(gen[ConstructParam]))

    case x: ScClassParameter =>
      val mod =
        if (x.isPrivate) PrivModifier
        else if (x.isPublic) PublModifier
        else NoModifier
      val t =
        if(x.isVal) ValType
        else if(x.isVar) VarType
        else NoType
      ConstructParam(t, mod, x.name, genType(x.typeElement, x.`type`()))

    case x: ScParameter =>
      DefParam(genType(x.typeElement, x.`type`()), x.name)

    case x =>
      println(s"No case for $x")
      EmptyAst
  }).asInstanceOf[T]
}
