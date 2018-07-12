package org.jetbrains.plugins.kotlinConverter

import com.intellij.psi.{PsiClass, PsiCodeBlock, PsiElement, PsiStatement}
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.types.ScalaTypes
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.base._
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns._
import org.jetbrains.plugins.scala.lang.psi.api.base.types.{ScSimpleTypeElement, ScTypeElement}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScClassParameter, ScParameter}
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.{ScImportExpr, ScImportStmt}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScClassParents
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScObject, ScTrait, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.expr.{ScBlockExprImpl, ScNewTemplateDefinitionImpl, ScReferenceExpressionImpl}
import org.jetbrains.plugins.scala.lang.psi.impl.statements.FakePsiStatement
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.TypeDefinitionMembers
import org.jetbrains.plugins.scala.lang.psi.light.PsiClassWrapper
import org.jetbrains.plugins.scala.lang.psi.types.{ScParameterizedType, ScType}
import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil
import org.scalafmt.internal.SyntacticGroup.Type.SimpleTyp

import scala.collection.immutable

object ASTGenerator extends App() with AST {
  private def genDefinitions(file: ScalaFile): Seq[PsiElement] = {
    println(file.typeDefinitions.mkString(", "))
    val functionDefns =
      file.findChildrenByType(ScalaElementTypes.FUNCTION_DEFINITION)
    val functionDecls =
      file.findChildrenByType(ScalaElementTypes.FUNCTION_DECLARATION)
    functionDefns ++ functionDecls ++ file.typeDefinitions
  }

  def exprToBlock(expr: Option[Expr]): BlockExpr =
    expr.map {
      case y: BlockExpr => y
      case y => SingleBlock(y)
    }.getOrElse(EmptyBlock)

  private def genFunctionBody(fun: ScFunction): BlockExpr = fun match {
    case x: ScFunctionDefinition =>
      exprToBlock(x.body.map(gen[Expr]))
    case _: ScFunctionDeclaration =>
      EmptyBlock
  }

  private def multiOrEmptyBlock(defs: Seq[DefExpr]): BlockExpr =
    if (defs.isEmpty) EmptyBlock
    else MultiBlock(defs)

  def genTypeArgs(genCall: ScGenericCall): Seq[TypeParam] = {
    genCall.typeArgs.
      map(_.typeArgs)
      .toSeq
      .flatten
      .map(z => TypeParam(z.getText))
  }

  def genType(t: ScType): Type = {
    t match {
      case x: ScParameterizedType if x.designator.canonicalText.startsWith(ScalaTypes.FUNCTION_PREFFIX) =>
        if (x.typeArguments.init.length == 1)
          FuncType(genType(x.typeArguments.head), genType(x.typeArguments.last))
        else
          FuncType(ProdType(x.typeArguments.init.map(genType)), genType(x.typeArguments.last))
      case x: ScParameterizedType =>
        PType(genType(x.designator), x.typeArguments.map(genType))
      case x =>
        SimpleType(x.canonicalText)
    }
  }

  def genType(t: Option[ScTypeElement]): Type =
    t.flatMap(_.`type`().toOption).map(genType)
      .getOrElse(NoType)

  //  def genType(t: Option[ScType]): Type =
  //    t.map(genType).getOrElse(NoType)

  def genType(t: TypeResult): Type =
    t.map(genType).getOrElse(NoType)

  //  def realOrInfType(r: Option[ScTypeElement], t: TypeResult) =
  //    r.map(x => genType(x.`type`()))
  //      .getOrElse(genType(t))


  def genAttrs(x: ScTypeDefinition): Seq[Attr] = {
    def attr(p: Boolean, a: Attr) =
      if (p) Some(a) else None

    (attr(x.isCase, CaseAttr) ::
      attr(x.isPrivate, PrivAttr) ::
      attr(x.isPublic, PublAttr) ::
      attr(x.isProtected, ProtAttr) ::
      attr(x.hasFinalModifier, FinalAttr) ::
      Nil
      ).flatten
  }

  def gen[T](psi: PsiElement): T = (psi match {
    case x: ScalaFile =>
      FileDef(
        x.getPackageName,
        x.importStatementsInHeader.flatMap(_.importExprs).map(gen[ImportDef]),
        genDefinitions(x)
          .filter {
            case _: PsiClassWrapper => false
            case y: ScObject if y.isSyntheticObject => false
            case _ => true
          }
          .map(gen[DefExpr]))
    case x: ScImportExpr =>
      ImportDef(x.reference.map(_.getText).get, x.importedNames)
    case x: ScTypeDefinition =>
      val construct = x match {
        case y: ScClass => Some(y.constructor.map(gen[Construct]).getOrElse(EmptyConstruct))
        case _ => None
      }
      //      construct.map {
      //        case ParamsConstruct(params) =>
      //          params.map(_.name)
      //        case _ => Seq.empty
      //      }
      //        .toSeq
      //        .flatten
      //        .filter { name =>
      //          val t = TypeDefinitionMembers.getSignatures(x).forName(ScalaNamesUtil.clean(name))._1
      //          t
      //          true
      //        }
      Defn(
        genAttrs(x),
        x match {
          case _: ScClass => ClassDefn
          case _: ScTrait => TraitDefn
          case _: ScObject => ObjDefn
        },
        x.name,
        construct,
        x.extendsBlock
          .findChildrenByType(ScalaElementTypes.CLASS_PARENTS)
          .flatMap { case y: ScClassParents =>
            y.findChildrenByType(ScalaElementTypes.CONSTRUCTOR)
              .map { case z: ScConstructor =>
                Super(genType(z.typeElement.`type`()), None)
              }
          },
        multiOrEmptyBlock(
          x.extendsBlock.members.map(gen[DefExpr])))
    case x: PsiClassWrapper =>
      gen[DefExpr](x.definition)

    case x: ScFunction =>
      //TODO get override modifier
      //      x.superSignatures
      DefnDef(
        Seq.empty,
        x.name,
        genType(x.`type`()),
        x.parameters.map(gen[DefParam]),
        genType(x.returnType),
        genFunctionBody(x))

    case x: ScBlock =>
      if (x.hasRBrace || x.statements.size > 1)
        MultiBlock(x.statements.map(gen[Expr]))
      else if (x.statements.isEmpty)
        EmptyBlock
      else
        SingleBlock(gen[Expr](x.statements.head))

    case x: ScInfixExpr =>
      BinExpr(genType(x.`type`()), BinOp(x.operation.getText), gen[Expr](x.left), gen[Expr](x.right))
    case x: ScLiteral =>
      LitExpr(genType(x.`type`()), x.getText)
    case x: ScUnderscoreSection =>
      UnderScExpr(NoType)
    case x: ScParenthesisedExpr =>
      ParenExpr(gen[Expr](x.innerElement.get))
    case x: ScReferenceExpression =>
      val isFunc =
        x.getReference.asInstanceOf[ScReferenceExpressionImpl]
          .shapeResolve
          .map(_.element)
          .exists(_.isInstanceOf[ScFunction])
      RefExpr(
        genType(x.`type`()),
        x.qualifier.map(gen[Expr]),
        x.refName,
        Seq.empty,
        isFunc)

    case x: ScMethodCall =>
      CallExpr(
        genType(x.`type`()),
        gen[Expr](x.getInvokedExpr),
        x.args.exprs.map(gen[Expr]))

    case x: ScGenericCall =>
      println("Hi")
      gen[RefExpr](x.referencedExpr).copy(typeParams = genTypeArgs(x))
//      RefExpr(
//        genType(x.`type`()),
//        Some(gen[Expr](x.referencedExpr)),
//        "",
//        genTypeArgs(x),
//        )
//      gen[CallExpr](x.referencedExpr)
//        .copy(typeParams = genTypeArgs(x))
//    //      CallExpr(
    //        genType(x.`type`()),
    //        x.getInvokedExpr match {
    //          case y: ScGenericCall =>
    //            gen[Expr](y.referencedExpr)
    //          case y => gen[Expr](y)
    //        },
    //        x.getInvokedExpr match {
    //          case y: ScGenericCall => genTypeArgs(y)
    //          case _ => Seq.empty
    //        },
    //        x.args.exprs.map(gen[Expr]))
    //    case x: ScGenericCall =>
    //      CallExpr(genType(x.`type`()),
    //        gen[Expr](x.referencedExpr),
    //        genTypeArgs(x),
    //        Seq.empty)
    case x: ScIfStmt =>
      IfExpr(
        genType(x.`type`()),
        gen[Expr](x.condition.get),
        exprToBlock(x.thenBranch.map(gen[Expr])),
        exprToBlock(x.elseBranch.map(gen[Expr])))
    //    case x: ScThrowStmt =>
    //         Throw(gen[Expr](x.body.get))
    case x: ScMatchStmt =>
      MatchExpr(genType(x.`type`()), gen[Expr](x.expr.get), x.caseClauses.map(gen[CaseClause]))
    case x: ScFunctionExpr =>
      LambdaExpr(genType(x.`type`()), x.parameters.map(gen[DefParam]), gen[Expr](x.result.get))
    case x: ScCaseClause =>
      CaseClause(gen[CasePattern](x.pattern.get), gen[Expr](x.expr.get))
    case x: ScLiteralPattern =>
      LitPattern(gen[LitExpr](x.getLiteral))
    case x: ScConstructorPattern =>
      ConstructorPattern(x.ref.qualName, x.args.patterns.map(gen[CasePattern]))
    case x: ScTypedPattern =>
      TypedPattern(x.name, genType(x.typePattern.map(_.typeElement)))
    case x: ScReferencePattern =>
      ReferencePattern(x.name)
    case x: ScReferenceElement =>
      ReferencePattern(x.refName)
    case x: ScStableReferenceElementPattern =>
      ReferencePattern(x.getReferenceExpression.get.refName)
    case _: ScWildcardPattern =>
      WildcardPattern
    case x: ScPatternDefinition =>
      ValDef(
        x.bindings.head.name,
        genType(x.typeElement),
        gen[Expr](x.expr.get))
    case x: ScVariableDefinition =>
      VarDef(
        x.bindings.head.name,
        genType(x.typeElement),
        gen[Expr](x.expr.get))
    case x: ScAssignStmt =>
      AssignExpr(gen[Expr](x.getLExpression), gen[Expr](x.getRExpression.get))
    case x: ScNewTemplateDefinitionImpl =>
      NewExpr(
        genType(x.`type`()),
        x.constructor.get.typeElement.getText,
        x.constructor.get.args.toSeq.flatMap(_.exprs).map(gen[Expr]))
    case x: ScPrimaryConstructor =>
      ParamsConstruct(x.parameters.map(gen[ConstructParam]))

    case x: ScClassParameter =>
      val mod =
        if (x.isPrivate) PrivAttr
        else PublAttr
      val t =
        if (x.isVal) VarlKind
        else if (x.isVar) VarKind
        else NoMemberKind
      ConstructParam(t, mod, x.name, genType(x.typeElement))

    case x: ScParameter =>
      DefParam(genType(x.typeElement), x.name)

    //    case x =>
    //      println(s"No case for $x")
    //      EmptyAst
  }).asInstanceOf[T]
}
