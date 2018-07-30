package org.jetbrains.plugins.kotlinConverter

import com.intellij.psi.{PsiClass, PsiCodeBlock, PsiElement, PsiStatement}
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.types.ScalaTypes
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.base._
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns._
import org.jetbrains.plugins.scala.lang.psi.api.base.types.{ScSimpleTypeElement, ScTypeArgs, ScTypeElement}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScClassParameter, ScParameter, ScTypeParam}
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.{ScImportExpr, ScImportStmt}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.{ScClassParents, ScTemplateParents}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef._
import org.jetbrains.plugins.scala.lang.psi.impl.expr.{ScBlockExprImpl, ScNewTemplateDefinitionImpl, ScReferenceExpressionImpl}
import org.jetbrains.plugins.scala.lang.psi.impl.statements.FakePsiStatement
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.TypeDefinitionMembers
import org.jetbrains.plugins.scala.lang.psi.light.PsiClassWrapper
import org.jetbrains.plugins.scala.lang.psi.types.api.TypeParameter
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.{DesignatorOwner, ScDesignatorType}
import org.jetbrains.plugins.scala.lang.psi.types.nonvalue.{ScMethodType, ScTypePolymorphicType}
import org.jetbrains.plugins.scala.lang.psi.types.{PhysicalSignature, ScParameterizedType, ScType}
import org.jetbrains.plugins.scala.lang.psi.types.result.{TypeResult, Typeable}
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil
import org.scalafmt.internal.SyntacticGroup.Type.SimpleTyp

import scala.collection.immutable

object ASTGenerator extends {
  private def genDefinitions(file: ScalaFile): Seq[PsiElement] =
    file.getChildren.filter {
      case _: ScFunction => true
      case _: ScVariable => true
      case _: ScValue => true
      case _ => false
    } ++ file.typeDefinitions


  private def genFunctionBody(fun: ScFunction): Option[Expr] = fun match {
    case x: ScFunctionDefinition =>
      x.body.map(gen[Expr])
    case _: ScFunctionDeclaration =>
      None
  }


  def genTypeArgs(typeArgs: Option[ScTypeArgs]): Seq[TypeParam] = {
    typeArgs
      .map(_.typeArgs)
      .toSeq
      .flatten
      .map(z => TypeParam(genType(z.`type`())))
  }

  def genType(ty: ScType): Type =
    ty match {
      case x: ScParameterizedType if x.designator.canonicalText.startsWith(ScalaTypes.FUNCTION_PREFFIX) =>
        if (x.typeArguments.init.length == 1)
          FuncType(genType(x.typeArguments.head), genType(x.typeArguments.last))
        else
          FuncType(ProdType(x.typeArguments.init.map(genType)), genType(x.typeArguments.last))
      case x: ScParameterizedType =>
        ProductType(genType(x.designator), x.typeArguments.map(genType))
      case x: ScTypePolymorphicType =>
        genType(x.internalType)
      case x: ScMethodType =>
        FuncType(ProdType(x.params.map(t => genType(t.paramType))), genType(x.returnType))
      case x: DesignatorOwner =>
        x.extractDesignatorSingleton.map(genType)
          .getOrElse(SimpleType(x.canonicalText))
      case x =>
        SimpleType(x.canonicalText)
    }


  def genType(t: Option[ScTypeElement]): Type =
    t.flatMap(_.`type`().toOption).map(genType)
      .getOrElse(NoType)

  def genType(t: TypeResult): Type =
    t.map(genType).getOrElse(NoType)

  def blockOrEmpty(exprs: Seq[Expr]): Option[BlockExpr] =
    if (exprs.nonEmpty) Some(BlockExpr(exprs.last.ty, exprs))
    else None

  def genAttrs(x: ScMember): Seq[Attr] = {
    def attr(p: Boolean, a: Attr) =
      if (p) Some(a) else None

    val memberAttrs = (attr(x.isPrivate, PrivAttr) ::
      attr(x.isPublic, PublAttr) ::
      attr(x.isProtected, ProtAttr) ::
      attr(x.hasFinalModifier, FinalAttr) ::
      attr(x.hasAbstractModifier, AbstractAttr) ::
      Nil).flatten
    val extraAttrs = x match {
      case y: ScFunction =>
        attr(y.superMethod.isDefined, OverrideAttr).toSeq
      case y: ScTypeDefinition =>
        (attr(y.isCase, CaseAttr) ::
          Nil).flatten

      case _ => Seq.empty
    }
    memberAttrs ++ extraAttrs
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
      if (x.isSingleWildcard)
        ImportDef(x.reference.map(_.getText).get, Seq("*"))
      else
        ImportDef(x.reference.map(_.getText).get, x.selectors.flatMap(_.importedName))

    case x: ScTypeDefinition =>
      x.typeParameters
      val construct = x match {
        case y: ScClass => Some(y.constructor.map(gen[Construct]).getOrElse(EmptyConstruct))
        case _ => None
      }

      val overrideConstuctParamsDefs =
        x match {
          case y: ScClass => y.constructor.toSeq.collect {
            case z: ScPrimaryConstructor =>
              z.parameters
                .filter(p => ScalaPsiUtil.superValsSignatures(p).nonEmpty)
          }.flatten
            .map { case p: ScClassParameter =>
              DefnDef(Seq(PublAttr, OverrideAttr),
                p.name,
                Seq.empty,
                genType(p.`type`()),
                Seq.empty,
                genType(p.`type`()),
                Some(RefExpr(genType(p.`type`()), None, p.name, Seq.empty, false)))
            }
          case _ => Seq.empty
        }

      Defn(
        genAttrs(x),
        x match {
          case _: ScClass => ClassDefn
          case _: ScTrait => TraitDefn
          case _: ScObject => ObjDefn
        },
        x.name,
        x.typeParameters.map(gen[TypeParam]),
        construct,
        x.extendsBlock.templateParents.map(gen[SupersBlock]),
        blockOrEmpty(
          overrideConstuctParamsDefs ++ x.extendsBlock.members.map(gen[DefExpr])))
    case x: PsiClassWrapper =>
      gen[DefExpr](x.definition)

    case x: ScTemplateParents =>
      val constructor = x match {
        case y: ScClassParents => y.constructor.map { c =>
          SuperConstructor(genType(c.typeElement.`type`()), c.args.toSeq.flatMap(_.exprs).map(gen[Expr]))
        }
        case _ => None
      }
      SupersBlock(constructor, x.typeElementsWithoutConstructor.map(_.`type`()).map(genType))

    case x: ScFunction =>
      DefnDef(
        genAttrs(x),
        x.name,
        x.typeParameters.map(z => TypeParam(SimpleType(z.typeParameterText))),
        genType(x.`type`()),
        x.parameters.map(gen[DefParam]),
        genType(x.returnType),
        genFunctionBody(x))

    case x: ScBlockExpr if x.hasCaseClauses =>
      LambdaExpr(genType(x.`type`()),
        Seq.empty,
        MatchExpr(genType(x.`type`()), UnderScExpr(NoType), x.caseClauses.get.caseClauses.map(gen[MatchCaseClause])),
        false)

    case x: ScBlock =>
      BlockExpr(genType(x.`type`()), x.statements.map(gen[Expr]))

    case x: ScInfixExpr =>
      BinExpr(genType(x.`type`()), x.operation.getText, gen[Expr](x.left), gen[Expr](x.right))
    case x: ScInterpolatedStringLiteral =>
      InterpolatedStringExpr(x.getStringParts, x.getInjections.map(gen[Expr]))
    case x: ScLiteral =>
      LitExpr(genType(x.`type`()), x.getText)
    case x: ScUnderscoreSection =>
      UnderScExpr(NoType)
    case x: ScParenthesisedExpr =>
      ParenExpr(gen[Expr](x.innerElement.get))

    case x: ScReferenceExpression =>
      val ty = x.multiType
        .headOption
        .flatMap(_.toOption)
        .map(genType)
        .getOrElse(genType(x.`type`()))
      val isFunc =
        x.getReference.asInstanceOf[ScReferenceExpressionImpl]
          .shapeResolve
          .map(_.element)
          .exists(_.isInstanceOf[ScFunction])
      RefExpr(
        ty,
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
      gen[RefExpr](x.referencedExpr).copy(typeParams = genTypeArgs(x.typeArgs))

    case x: ScIfStmt =>
      IfExpr(
        genType(x.`type`()),
        gen[Expr](x.condition.get),
        gen[Expr](x.thenBranch.get),
        x.elseBranch.map(gen[Expr]))

    case x: ScMatchStmt =>
      MatchExpr(genType(x.`type`()), gen[Expr](x.expr.get), x.caseClauses.map(gen[MatchCaseClause]))
    case x: ScFunctionExpr =>
      LambdaExpr(genType(x.`type`()), x.parameters.map(gen[DefParam]), gen[Expr](x.result.get), false)

    case x: ScCaseClause =>
      MatchCaseClause(gen[MatchCasePattern](x.pattern.get),
        x.expr.map(gen[Expr]).get, //todo fix
        x.guard.flatMap(_.expr).map(gen[Expr]))

    case x: ScCompositePattern =>
      CompositePatternMatch(x.subpatterns.map(gen[MatchCasePattern]))
    case x: ScLiteralPattern =>
      LitPatternMatch(gen[LitExpr](x.getLiteral))
    case x: ScNamingPattern =>
      gen[ConstructorPatternMatch](x.named).copy(label = Some(x.name))
    case x: ScConstructorPattern =>
      ConstructorPatternMatch(x.ref.qualName, x.args.patterns.map(gen[MatchCasePattern]), None, x.getText)
    case x: ScTypedPattern =>
      TypedPatternMatch(x.name, genType(x.typePattern.map(_.typeElement)))
    case x: ScReferencePattern =>
      ReferencePatternMatch(x.name)
    case x: ScReferenceElement =>
      ReferencePatternMatch(x.refName)
    case x: ScStableReferenceElementPattern =>
      LitPatternMatch(gen[Expr](x.getReferenceExpression.get))
    case _: ScWildcardPattern =>
      WildcardPatternMatch
    case x: ScPatternDefinition =>
      ValDef(
        x.pList.patterns.map(gen[MatchCasePattern]),
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
      val kind =
        if (x.isVal) ValKind
        else if (x.isVar) VarKind
        else NoMemberKind

      val modifier = kind match {
        case NoMemberKind => NoAttr
        case _ =>
          if (x.isPrivate) PrivAttr
          else if (x.isProtected) ProtAttr
          else if (x.hasModifierProperty("public")) PublAttr
          else NoAttr
      }


      ConstructParam(kind, modifier, x.name, genType(x.typeElement))

    case x: ScParameter =>
      DefParam(genType(x.typeElement), x.name)

    case x: ScTryStmt =>
      TryExpr(gen[Expr](x.tryBlock), x.finallyBlock.flatMap(_.expression).map(gen[Expr]))

    case x: ScForStatement =>
      ForExpr(
        genType(x.`type`()),
        x.enumerators.toSeq.flatMap(_.getChildren).map(gen[ForEnumerator]),
        gen[Expr](x.body.get)
      )
    case x: ScGenerator =>
      ForGenerator(gen[MatchCasePattern](x.pattern), gen[Expr](x.rvalue))

    case x: ScGuard =>
      ForGuard(gen[Expr](x.expr.get))

    case x: ScEnumerator =>
      ForVal(gen[MatchCasePattern](x.pattern), gen[Expr](x.rvalue))

    case x: ScTypeParam =>
      TypeParam(SimpleType(x.typeParameterText)) //todo improve

    case x: ScThisReference =>
      ThisExpr(genType(x.`type`()))

  }).asInstanceOf[T]
}
