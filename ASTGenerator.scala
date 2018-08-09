package org.jetbrains.plugins.kotlinConverter

import com.intellij.psi.{PsiClass, PsiCodeBlock, PsiElement, PsiStatement}
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.builder.codegen.{Definition, TupleDefinition}
import org.jetbrains.plugins.kotlinConverter.scopes.{ASTGeneratorState, ScopedVal}
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
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.{ScClassImpl, TypeDefinitionMembers}
import org.jetbrains.plugins.scala.lang.psi.light.PsiClassWrapper
import org.jetbrains.plugins.scala.lang.psi.types.api.{StdType, TypeParameter, TypeParameterType}
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.{DesignatorOwner, ScDesignatorType, ScProjectionType, ScThisType}
import org.jetbrains.plugins.scala.lang.psi.types.nonvalue.{ScMethodType, ScTypePolymorphicType}
import org.jetbrains.plugins.scala.lang.psi.types.{PhysicalSignature, ScExistentialArgument, ScExistentialType, ScParameterizedType, ScType}
import org.jetbrains.plugins.scala.lang.psi.types.result.{TypeResult, Typeable}
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil
import org.scalafmt.internal.SyntacticGroup.Type.SimpleTyp
import org.jetbrains.plugins.kotlinConverter.scopes.ScopedVal.scoped
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTemplateDefinition.Kind.ScObject

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.Try

object ASTGenerator extends Collector {
  val stateVal = new ScopedVal[ASTGeneratorState](ASTGeneratorState(Map.empty))

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


  def genTypeArgs(typeArgs: Option[ScTypeArgs]): Seq[Type] = {
    typeArgs
      .map(_.typeArgs)
      .toSeq
      .flatten
      .map(z => genType(z.`type`()))
  }

  def genType(ty: ScType): Type =
    ty match {
      case x: StdType =>
        ScalaStdType(x.name)
      case x: ScParameterizedType if x.designator.canonicalText.startsWith(ScalaTypes.FUNCTION_PREFFIX) =>
        if (x.typeArguments.init.length == 1)
          FunctionType(genType(x.typeArguments.head), genType(x.typeArguments.last))
        else
          FunctionType(ProductType(x.typeArguments.init.map(genType)), genType(x.typeArguments.last))
      case x: ScParameterizedType =>
        GenericType(genType(x.designator), x.typeArguments.map(genType))
      case x: ScTypePolymorphicType =>
        genType(x.internalType)
      case x: ScMethodType =>
        FunctionType(ProductType(x.params.map(t => genType(t.paramType))), genType(x.returnType))
      case x: DesignatorOwner =>
        x.extractClass.map {
          case c: ScClassImpl => ClassType(c.qualifiedName)
          case c: PsiClass => ClassType(c.getQualifiedName)
        }.orElse {
          x.extractDesignatorSingleton.map(genType)
        }.get //OrElse(SimpleType(x.canonicalText))
      case x: ScProjectionType =>
        genType(x.projected)
      case x: ScThisType =>
        genType(x.element.`type`())
      case x: ScExistentialType =>
        genType(x.quantified)
      case x: TypeParameterType =>
        TypeParamType(TypeParam(x.typeParameter.name))
      case x: ScExistentialArgument =>
        SimpleType("*")
      //      case x =>
      //        SimpleType(x.canonicalText)
    }


  def genType(t: Option[ScTypeElement]): Type =
    t.flatMap(_.`type`().toOption).map(genType)
      .getOrElse(NoType)

  def genType(t: TypeResult): Type =
    t.map(genType).getOrElse(NoType)

  def blockOrEmpty(exprs: Seq[Expr]): Option[BlockExpr] =
    if (exprs.nonEmpty) Some(BlockExpr(exprs.last.exprType, exprs))
    else None

  def genAttributes(x: ScMember): Seq[Attribute] = {
    def attr(p: Boolean, a: Attribute) =
      if (p) Some(a) else None

    val memberAttrs = (attr(x.isPrivate, PrivateAttribute) ::
      attr(x.isPublic, PublicAttribute) ::
      attr(x.isProtected, ProtectedAttribute) ::
      attr(x.hasFinalModifier, FinalAttribute) ::
      attr(x.hasAbstractModifier, AbstractAttribute) ::
      Nil).flatten
    val extraAttrs = x match {
      case y: ScFunction =>
        attr(y.superMethod.isDefined, OverrideAttribute).toSeq
      case y: ScTypeDefinition =>
        (attr(y.isCase, CaseAttribute) ::
          Nil).flatten

      case _ => Seq.empty
    }
    memberAttrs ++ extraAttrs
  }

  def gen[T](psi: PsiElement): T =
    stateVal.precalculated.get(psi.getTextRange)
      .map(_.asInstanceOf[T])
      .getOrElse(recover[T](psi))


  def findUnderscores(expr: PsiElement): Seq[ScUnderscoreSection] = {
    if (expr.getText.indexOf('_') == -1) Seq.empty
    else inner(expr)

    def inner(innerExpr: PsiElement): Seq[ScUnderscoreSection] = {
      innerExpr match {
        case under: ScUnderscoreSection =>
          Seq(under)
        case _ =>
          innerExpr.getChildren.flatMap(inner)
      }
    }

    inner(expr)
  }

  def recover[T](psi: PsiElement): T =
    Try(transform[T](psi))
      .recoverWith { case _ => Try(ErrorExpr(psi.getText).asInstanceOf[T]) }
      .recoverWith { case _ => Try(ErrorCasePattern(psi.getText).asInstanceOf[T]) }
      .recoverWith { case _ => Try(ErrorType(psi.getText).asInstanceOf[T]) }
      .recoverWith { case _ => Try(ErrorForEnumerator(psi.getText).asInstanceOf[T]) }
      .recoverWith { case _ => Try(ErrorWhenClause(psi.getText).asInstanceOf[T]) }
      .get

  def transform[T](psi: PsiElement): T = (psi match {
    case x: ScalaFile => //todo x --> sth else
      val underscores =
        findUnderscores(x).flatMap(_.overExpr).map { over =>
          val expr = gen[Expr](over)
          val lambdaExpr = LambdaExpr(expr.exprType, Seq.empty, expr, false)
          over.getTextRange -> lambdaExpr
        }.toMap
      scoped(
        stateVal.set(ASTGeneratorState(underscores))
      ) {
        File(
          x.getPackageName,
          Set.empty,
          //        x.importStatementsInHeader.flatMap(_.importExprs).map(gen[ImportDef]),
          genDefinitions(x)
            .filter {
              case _: PsiClassWrapper => false
              case y: ScObject if y.isSyntheticObject => false
              case _ => true
            }
            .map(gen[DefExpr])
            .filter {
              case Defn(_, ObjDefn, _, _, _, _, _, Some(_)) => false
              case _ => true
            },
          collectedDefinitions)
      }

    case x: ScTypeDefinition =>
      val construct = x match {
        case y: ScClass => Some(y.constructor.map(gen[Constructor]).getOrElse(EmptyConstructor))
        case _ => None
      }

      val overrideConstuctParamsDefs =
        x match {
          case y: ScClass => y.constructor.toSeq.collect {
            case z: ScPrimaryConstructor =>
              z.parameters
                .filter(p => ScalaPsiUtil.superValsSignatures(p).nonEmpty)
          }.flatten
            .map { p: ScClassParameter =>
              DefnDef(Seq(PublicAttribute, OverrideAttribute),
                p.name,
                Seq.empty,
                Seq.empty,
                genType(p.`type`()),
                Some(RefExpr(genType(p.`type`()), None, p.name, Seq.empty, false)))
            }
          case _ => Seq.empty
        }

      val defnType =
        x match {
          case _: ScClass => ClassDefn
          case _: ScTrait => TraitDefn
          case _: ScObject => ObjDefn
        }


      val companionDefn = x.baseCompanionModule.map {
        case _ if defnType == ObjDefn => ObjectCompanion
        case c => ClassCompanion(gen[Defn](c))
      }

      Defn(
        genAttributes(x),
        defnType,
        x.name,
        x.typeParameters.map(gen[TypeParam]),
        construct,
        x.extendsBlock.templateParents.map(gen[SupersBlock]),
        blockOrEmpty(
          overrideConstuctParamsDefs ++ x.extendsBlock.members.map(gen[DefExpr])),
        companionDefn)

    case x: PsiClassWrapper => //todo get rid of
      gen[DefExpr](x.definition)

    case x: ScTemplateParents =>
      val constructor = x match {
        case y: ScClassParents => y.constructor.map { c =>
          val needBrackets =
            x.typeElements
              .headOption
              .flatMap(_.`type`().toOption)
              .collect {
                case d: DesignatorOwner => d.extractClass
              }
              .flatten
              .exists(!_.isInstanceOf[ScTrait])
          SuperConstructor(genType(c.typeElement.`type`()), c.args.toSeq.flatMap(_.exprs).map(gen[Expr]), needBrackets)
        }
        case _ => None
      }
      SupersBlock(constructor, x.typeElementsWithoutConstructor.map(_.`type`()).map(genType))

    case x: ScFunction =>
      DefnDef(
        genAttributes(x),
        x.name,
        x.typeParameters.map(gen[TypeParam]),
        x.parameters.map(gen[DefParameter]),
        genType(x.returnType),
        genFunctionBody(x))

    case x: ScBlockExpr if x.isAnonymousFunction =>
      LambdaExpr(genType(x.`type`()),
        Seq.empty,
        MatchExpr(genType(x.`type`()), UnderscoreExpr(NoType), x.caseClauses.get.caseClauses.map(gen[MatchCaseClause])),
        false)

    case x: ScBlockExpr if x.isInCatchBlock =>
      ScalaCatch(x.caseClauses.get.caseClauses.map(gen[MatchCaseClause]))

    case x: ScBlock =>
      BlockExpr(genType(x.`type`()), x.statements.map(gen[Expr]))

    case x: ScInfixExpr =>
      InfixExpr(genType(x.`type`()),
        gen[RefExpr](x.operation),
        gen[Expr](x.left),
        gen[Expr](x.right),
        x.isLeftAssoc)

    case x: ScInterpolatedStringLiteral =>
      InterpolatedStringExpr(x.getStringParts, x.getInjections.map(gen[Expr]))
    case x: ScLiteral =>
      LitExpr(genType(x.`type`()), x.getText)
    case x: ScUnderscoreSection =>
      UnderscoreExpr(NoType)
    case x: ScParenthesisedExpr =>
      ParenthesesExpr(gen[Expr](x.innerElement.get))

    case x: ScReferenceExpression =>
      val ty = x.multiType
        .headOption
        .flatMap(_.toOption)
        .map(genType)
        .getOrElse(genType(x.`type`()))
      val isFunc =
        x.getReference.asInstanceOf[ScReferenceExpressionImpl]
          .shapeResolve //todo use bind
          .map(_.element)
          .exists(_.isInstanceOf[ScFunction])
      RefExpr(
        ty,
        x.qualifier.map(gen[Expr]),
        x.refName,
        Seq.empty,
        isFunc)

    case x: ScMethodCall =>
      val paramsInfo =
        x.args.callReference.flatMap(_.bind()).map(_.element).toSeq.flatMap {
          case f: ScFunction => f.parameters.map { p =>
            CallParameterInfo(genType(p.typeElement), p.isCallByNameParameter)
          }
          case _ =>
            Seq.fill(x.args.exprs.length)(CallParameterInfo(NoType, false))
        }


      CallExpr(
        genType(x.`type`()),
        gen[Expr](x.getInvokedExpr),
        x.args.exprs.map(gen[Expr]),
        paramsInfo)

    case x: ScGenericCall =>
      gen[RefExpr](x.referencedExpr).copy(typeParams = genTypeArgs(x.typeArgs))


    case psi: ScTypedStmt if psi.isSequenceArg =>
      PrefixExpr(genType(psi.`type`()), gen[Expr](psi.expr), "*")

    case psi: ScTypedStmt =>
      Exprs.as(gen[Expr](psi.expr), genType(psi.typeElement))

    case x: ScIfStmt =>
      IfExpr(
        genType(x.`type`()),
        gen[Expr](x.condition.get),
        gen[Expr](x.thenBranch.get),
        x.elseBranch.map(gen[Expr]))

    case x: ScMatchStmt =>
      MatchExpr(genType(x.`type`()), gen[Expr](x.expr.get), x.caseClauses.map(gen[MatchCaseClause]))
    case x: ScFunctionExpr =>
      LambdaExpr(genType(x.`type`()), x.parameters.map(gen[DefParameter]), gen[Expr](x.result.get), false)

    case x: ScCaseClause =>
      MatchCaseClause(gen[CasePattern](x.pattern.get),
        x.expr.map(gen[Expr]).get, //todo fix
        x.guard.flatMap(_.expr).map(gen[Expr]))

    case x: ScTuplePattern =>
      val arity = x.patternList.toSeq.flatMap(_.patterns).size
      addDefinition(new TupleDefinition(arity))
      ConstructorPattern(
        CaseClassConstructorRef(s"Tuple$arity"),
        x.patternList.toSeq.flatMap(_.patterns.map(gen[CasePattern])),
        None,
        x.getText)

    case x: ScCompositePattern =>
      CompositePattern(x.subpatterns.map(gen[CasePattern]))
    case x: ScLiteralPattern =>
      LitPattern(gen[LitExpr](x.getLiteral))
    case x: ScNamingPattern =>
      gen[ConstructorPattern](x.named).copy(label = Some(x.name))
    case x: ScConstructorPattern =>
      val bindResult = x.ref.bind()
      val obj = bindResult.flatMap(_.getActualElement match {
        case o: ScObject => Some(o)
        case _ => None
      })
      val unapplyRef = bindResult.flatMap(_.element match {
        case f: ScFunction => Some(f)
        case _ => None
      })
      val isCaseClass =
        obj.flatMap(_.baseCompanionModule).exists(_.isCase)
      val constuctorRef = (obj, unapplyRef) match {
        case (Some(o), Some(r)) if !isCaseClass =>
          UnapplyCallConstuctorRef(o.name, genType(r.returnType))
        case _ =>
          CaseClassConstructorRef(x.ref.qualName)
      }
      ConstructorPattern(constuctorRef, x.args.patterns.map(gen[CasePattern]), None, x.getText)


    case x: ScTypedPattern =>
      TypedPattern(x.name, genType(x.typePattern.map(_.typeElement)))
    case x: ScReferencePattern =>
      x.expectedType.map(genType) match {
        case Some(t) => TypedPattern(x.name, t)
        case _ => ReferencePattern(x.name)
      }

    case x: ScReferenceElement =>
      ReferencePattern(x.refName)
    case x: ScStableReferenceElementPattern =>
      LitPattern(gen[Expr](x.getReferenceExpression.get))
    case _: ScWildcardPattern =>
      WildcardPattern

    case x: ScPatternDefinition if x.isSimple =>
      SimpleValOrVarDef(
        genAttributes(x),
        true,
        x.pList.patterns.head.getText,
        Some(genType(x.pList.patterns.head.`type`())),
        x.expr.map(gen[Expr])
      )
    case x: ScPatternDefinition =>
      ValOrVarDef(
        genAttributes(x),
        true,
        x.pList.patterns.map(gen[CasePattern]),
        x.expr.map(gen[Expr]))

    case x: ScVariableDefinition if x.isSimple =>
      SimpleValOrVarDef(
        genAttributes(x),
        false,
        x.pList.patterns.head.getText,
        Some(genType(x.pList.patterns.head.`type`())),
        x.expr.map(gen[Expr])
      )

    case x: ScValueDeclaration =>
      SimpleValOrVarDef(
        genAttributes(x),
        true,
        x.declaredElements.head.name,
        x.declaredType.map(genType),
        None)

    case x: ScVariableDeclaration =>
      SimpleValOrVarDef(
        genAttributes(x),
        false,
        x.declaredElements.head.name,
        x.declaredType.map(genType),
        None)

    case x: ScAssignStmt =>
      AssignExpr(gen[Expr](x.getLExpression), gen[Expr](x.getRExpression.get))

    case x: ScNewTemplateDefinitionImpl =>
      NewExpr(
        genType(x.`type`()),
        genType(Some(x.constructor.get.typeElement)),
        x.constructor.get.args.toSeq.flatMap(_.exprs).map(gen[Expr]))

    case x: ScPrimaryConstructor =>
      ParamsConstructor(x.parameters.map(gen[ConstructorParam]))

    case x: ScClassParameter =>
      val kind =
        if (x.isVal) ValKind
        else if (x.isVar) VarKind
        else NoMemberKind

      val modifier = kind match {
        case NoMemberKind => NoAttribute
        case _ =>
          if (x.isPrivate) PrivateAttribute
          else if (x.isProtected) ProtectedAttribute
          else if (x.hasModifierProperty("public")) PublicAttribute
          else NoAttribute
      }


      ConstructorParam(kind, modifier, x.name, genType(x.typeElement))

    case x: ScParameter =>
      DefParameter(genType(x.typeElement), x.name, x.isVarArgs, x.isCallByNameParameter)

    case x: ScTryStmt =>
      ScalaTryExpr(genType(x.`type`()),
        gen[Expr](x.tryBlock),
        x.catchBlock.flatMap(_.expression).map(gen[ScalaCatch]),
        x.finallyBlock.flatMap(_.expression).map(gen[Expr]))

    case x: ScForStatement =>
      ForExpr(
        genType(x.`type`()),
        x.enumerators.toSeq.flatMap(_.getChildren).map(gen[ForEnumerator]),
        x.isYield,
        gen[Expr](x.body.get))

    case x: ScGenerator =>
      ForGenerator(gen[CasePattern](x.pattern), gen[Expr](x.rvalue))

    case x: ScGuard =>
      ForGuard(gen[Expr](x.expr.get))

    case x: ScEnumerator =>
      ForVal(ast.SimpleValOrVarDef(Seq.empty, true, x.pattern.getText, None, Some(gen[Expr](x.rvalue))))

    case x: ScTypeParam =>
      TypeParam(x.name)

    case x: ScPostfixExpr =>
      PostfixExpr(genType(x.`type`()), gen[Expr](x.operand), x.operation.refName)

    case x: ScThisReference =>
      ThisExpr(genType(x.`type`()))

  }).asInstanceOf[T]

}