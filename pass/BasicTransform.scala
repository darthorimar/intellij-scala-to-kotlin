package org.jetbrains.plugins.kotlinConverter.pass

import com.intellij.formatting.BlockEx
import com.sun.source.doctree.AttributeTree.ValueKind
import org.jetbrains.plugins.kotlinConverter
import org.jetbrains.plugins.kotlinConverter.{ExprUtils, Exprs, Utils}
import org.jetbrains.plugins.kotlinConverter.types.{KotlinTypes, StdTypes}
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.scopes.ScopedVal.scoped
import org.jetbrains.plugins.kotlinConverter.scopes.{BasicTransformState, LocalNamer, Renamer, ScopedVal}
import org.jetbrains.plugins.kotlinConverter.scopes.ScopedVal.scoped
import org.jetbrains.plugins.scala.conversion.ast.ClassConstruction

import scala.util.Try


class BasicTransform extends Transform {

  val stateVal: ScopedVal[BasicTransformState] = new ScopedVal[BasicTransformState](BasicTransformState(false))

  import org.jetbrains.plugins.kotlinConverter.types.TypeUtils._

  def isDefaultOperator(op: RefExpr): Boolean =
    op match {
      case RefExpr(FunctionType(NumericType(_), NumericType(_)), None, "*" | "/" | "+" | "-" | "%", _, _) => true
      case RefExpr(FunctionType(NumericType(_), StdTypes.BOOLEAN), None, ">" | "<" | ">=" | "<=" , _, _) => true
      case RefExpr(FunctionType(_, StdTypes.BOOLEAN), None, "==" | "!=" , _, _) => true
      case RefExpr(FunctionType(StdTypes.STRING, StdTypes.STRING), None, "+", _, _) => true
      case _ => false
    }

  override protected def action(ast: AST): Option[AST] = {
    ast match {
      case x@InfixExpr(exprType, op, _, _, _) if !isDefaultOperator(op) =>
        val (left, right) = (transform[Expr](x.left), transform[Expr](x.right))
        Some(transform[Expr](
          CallExpr(
            exprType,
            op.copy(referencedObject = Some(left)),
            Seq(transform[Expr](right)),
            Seq.empty
          )))

      //scala try --> kotlin try
      case ScalaTryExpr(exprType, tryBlock, catchBlock, finallyBlock) =>
        val cases = MatchUtils.expandCompositePattern(catchBlock.toSeq.flatMap(_.cases))
        val (goodClauses, badClauses) = cases.span {
          case MatchCaseClause(_: TypedPattern, _, None) => true
          case MatchCaseClause(_: ReferencePattern, _, None) => true
          case _ => false
        }
        val goodCatches = goodClauses.map {
          case MatchCaseClause(TypedPattern(referenceName, patternType, _), expr, None) =>
            KotlinCatchCase(referenceName, patternType, expr)
          case MatchCaseClause(ReferencePattern(referenceName, _), expr, None) =>
            KotlinCatchCase(referenceName, KotlinTypes.THROWABLE, expr)
        }
        val badCatches = badClauses match {
          case Seq() => Seq.empty
          case _ =>
            val ref = Exprs.simpleRef(namerVal.newName("e"), KotlinTypes.THROWABLE)
            val matchExpr = ExprUtils.blockOf(MatchUtils.convertMatchToWhen(ref, badClauses, exprType, this))
            Seq(KotlinCatchCase(ref.referenceName, ref.exprType, matchExpr))
        }
        Some(KotlinTryExpr(exprType,
          transform[Expr](tryBlock),
          (goodCatches ++ badCatches).map(transform[KotlinCatchCase]),
          finallyBlock.map(transform[Expr])))


      //rename refs
      case x: RefExpr if renamerVal.call(_.renames.contains(x.referenceName)) =>
        Some(renamerVal.get.renames(x.referenceName))

      //Remove renault brackets for lambda like in seq.map {x => x * 2}
      case BlockExpr(_, stmts) if stmts.size == 1 && stmts.head.isInstanceOf[LambdaExpr] =>
        Some(transform[Expr](stmts.head))

      case ParamsConstructor(params) =>
        Some(ParamsConstructor(params.map {
          case ConstructorParam(parType, mod, name, exprType) =>
            val t = if (parType == NoMemberKind) ValKind else parType
            val m =
              if (parent.asInstanceOf[Defn].attributes.exists(a => a == DataAttribute | a == CaseAttribute) &&
                (mod == PublicAttribute || mod == NoAttribute))
                NoAttribute
              else if (mod == NoAttribute)
                PrivateAttribute
              else mod
            ConstructorParam(t, m, name, transform[Type](exprType))
        }))


      case ForExpr(exprType, generators, isYield, body) =>
        def wrapToBody(expr: Expr) = expr match {
          case x: BlockExpr => x
          case _ => BlockExpr(expr.exprType, Seq(expr))
        }

        val yieldedBody =
          if (isYield)
            Exprs.simpleCall("yield", exprType, Seq(body))
          else body


        val result = generators.reverse.foldLeft(transform[Expr](yieldedBody): Expr) {
          case (acc, ForGenerator(pattern, expr)) =>
            ForInExpr(NoType, RefExpr(NoType, None, pattern.representation, Seq.empty, false), transform[Expr](expr), wrapToBody(acc))
          case (acc, ForGuard(condition)) =>
            IfExpr(NoType, transform[Expr](condition), wrapToBody(acc), None)
          case (acc, ForVal(valDefExpr)) =>
            BlockExpr(NoType, Seq(
              transform[Expr](valDefExpr),
              acc))
        }
        if (isYield) {
          imports = imports + Import("kotlin.coroutines.experimental.buildSequence")
          Some(
            Exprs.simpleCall("buildSequence",
              exprType,
              Seq(LambdaExpr(exprType, Seq.empty, result, false))))
        } else Some(result)


      // sort fun attrs, add return to the function end
      case x: DefnDef =>
        val renames = x.parameters.collect {
          case p: DefParameter if p.isCallByName =>
            p.name -> Exprs.simpleCall(p.name, p.parameterType, Seq.empty)
        }
        scoped(
          namerVal.set(new LocalNamer),
          renamerVal.updated(_.addAll(renames.toMap))
        ) {
          val newDef = copy(x).asInstanceOf[DefnDef]

          def handleBody(body: Expr) = body match {
            case b@BlockExpr(exprType, stmts) =>
              val last = stmts.last
              if (!last.isInstanceOf[ReturnExpr] && x.returnType != StdTypes.UNIT)
                BlockExpr(exprType, stmts.init :+ ReturnExpr(None, Some(last)))
              else b
            case b => b
          }

          val params = newDef.parameters.map {
            case DefParameter(parameterType, name, isVarArg, true) =>
              DefParameter(FunctionType(StdTypes.UNIT, parameterType), name, isVarArg, true)
            case p => p
          }

          Some(newDef.copy(
            attributes = handleAttrs(newDef),
            body = newDef.body.map(handleBody),
            parameters = params))
        }


      case x: ValOrVarDef =>
        Some(copy(x).asInstanceOf[ValOrVarDef].copy(attributes = handleAttrs(x)))

      case x: SimpleValOrVarDef =>
        Some(copy(x).asInstanceOf[SimpleValOrVarDef].copy(attributes = handleAttrs(x)))

      case x: Defn =>
        scoped(
          stateVal.updated { s =>
            BasicTransformState(s.inCompanionObject || x.isObjectDefn && x.defnType == ObjDefn)
          }
        ) {

          val defn = copy(x).asInstanceOf[Defn]

          def generateFakeCompanion: Defn = {
            val defnType = ClassType(defn.name)
            val applyDef = {
              val parameters = defn.constructor
                .collect {
                  case ParamsConstructor(ps) => ps
                }
                .toSeq
                .flatten
                .map { case ConstructorParam(_, _, paramName, parameterType) =>
                  DefParameter(parameterType, paramName, false, false)
                }
              val arguments =
                parameters.map { case DefParameter(parameterType,paramName , _, _) =>
                  Exprs.simpleRef(paramName, parameterType)
                }
              val body = NewExpr(defnType, arguments)
              DefnDef(Seq.empty, "apply", Seq.empty, parameters, defnType, Some(body))
            }
            val unapplyDef = {
              val parameters =
                Seq(DefParameter(defnType, "x", false, false))
              val body = Exprs.simpleRef("x", defnType)
              DefnDef(Seq.empty, "unapply", Seq.empty, parameters, NullableType(defnType), Some(body))
            }
            Defn(Seq(CompanionAttribute), ObjDefn, "", Seq.empty, None, None, Some(BlockExpr(NoType, Seq(applyDef, unapplyDef))), None)
          }

          val companionObj =
            defn.companionDefn.flatMap {
              case ClassCompanion(c) =>
                Some(c.copy(attributes = c.attributes :+ CompanionAttribute))
              case _ => None
            }.orElse {
              if (x.attributes.contains(CaseAttribute) | x.attributes.contains(DataAttribute)) {
                Some(generateFakeCompanion)
              } else None
            }.toSeq
          val exprs = defn.body.toSeq.flatMap(_.exprs) ++ companionObj
          val newBody =
            if (exprs.isEmpty) None
            else Some(BlockExpr(NoType, exprs))

          val defnType =
            if (defn.defnType == TraitDefn) InterfaceDefn
            else defn.defnType

          val name =
            if (defn.companionDefn.contains(ObjectCompanion)) ""
            else defn.name

          Some(copy(defn).asInstanceOf[Defn]
            .copy(attributes = handleAttrs(defn),
              defnType = defnType,
              body = newBody,
              name = name))
        }

      //uncarry
      case x@CallExpr(_, c: CallExpr, _, _) if c.exprType.isFunction =>
        def collectParams(c: Expr): List[Expr] = c match {
          case x: CallExpr if x.ref.exprType.isFunction =>
            collectParams(x.ref) ++ x.params.toList
          case _ => Nil
        }

        def collectRef(c: CallExpr): Expr = c.ref match {
          case x: CallExpr => collectRef(x)
          case x => x
        }

        val params = collectParams(x)
        val ref = collectRef(x)
        Some(CallExpr(
          transform[Type](x.exprType),
          copy(ref).asInstanceOf[RefExpr],
          params.map(transform[Expr]),
          Seq.empty))


      //a.foo(f) --> a.foo{f(it)}
      //a.foo(_ + 1) --> a.foo {it + 1}
      //handle call by name params
      case CallExpr(exprType, ref, params, paramsExpectedTypes) =>
        val paramsInfo =
          paramsExpectedTypes ++ Seq.fill(params.length - paramsExpectedTypes.length)(CallParameterInfo(NoType, false))
        Some(
          CallExpr(
            transform[Type](exprType),
            transform[Expr](ref),
            params.zip(paramsInfo).map {
              case (y: RefExpr, CallParameterInfo(_: FunctionType, _)) if y.isFunctionRef =>
                LambdaExpr(
                  exprType,
                  Seq.empty,
                  CallExpr(transform[Type](y.exprType), transform[Expr](y), Seq(UnderscoreExpr(y.exprType)), Seq.empty),
                  false)
              case (y@RefExpr(exprType, obj, ref, typeParams, true), _) =>
                CallExpr(exprType, transform[Expr](y), Seq.empty, Seq.empty)
              case (y, _) => transform[Expr](y)
            }.zip(paramsInfo).map {
              case (e, CallParameterInfo(_, true)) =>
                LambdaExpr(e.exprType, Seq.empty, e, false)
              case (e, _) => e
            },
            Seq.empty))

      //x.foo --> x.foo()
      case x@RefExpr(exprType, obj, ref, typeParams, true)
        if !parent.isInstanceOf[CallExpr] =>
        Some(CallExpr(exprType, copy(x).asInstanceOf[RefExpr], Seq.empty, Seq.empty))


      //foo.apply(sth) --> foo(sth)
      case x@RefExpr(exprType, Some(obj), "apply", typeParams, _)
        if parent.isInstanceOf[CallExpr] && obj.exprType.isFunction =>
        Some(transform[Expr](obj))

      // matchExpr to when one
      case MatchExpr(exprType, expr, clauses) =>
        val newExpr = transform[Expr](expr)
        val valExpr = SimpleValOrVarDef(Seq.empty, true, namerVal.newName("match"), None, Some(newExpr))
        val valRef = RefExpr(newExpr.exprType, None, valExpr.name, Seq.empty, false)
        val whenExpr = MatchUtils.convertMatchToWhen(valRef, clauses, exprType, this)
        Some(ExprUtils.blockOf(valExpr +: whenExpr))

      case _ => None
    }
  }


  private def handleAttrs(x: DefExpr): Seq[Attribute] = {
    def attr(p: Boolean, a: Attribute) =
      if (p) Some(a) else None

    val isOpen =
      !x.attributes.contains(FinalAttribute) &&
        x.isClassDefn && !x.attributes.contains(CaseAttribute) &&
        !x.attributes.contains(AbstractAttribute)

    val accessModifier =
      if (!x.isDefn && x.attributes.contains(PrivateAttribute) && stateVal.inCompanionObject)
        Some(InternalAttribute)
      else if (x.attributes.contains(PrivateAttribute)) Some(PrivateAttribute)
      else if (x.attributes.contains(ProtectedAttribute)) Some(ProtectedAttribute)
      else None
    val attrs =
      (attr(x.attributes.contains(CaseAttribute) && x.isClassDefn, DataAttribute) ::
        attr(isOpen, OpenAttribute) ::
        accessModifier ::
        attr(x.attributes.contains(AbstractAttribute), AbstractAttribute) ::
        attr(x.attributes.contains(OverrideAttribute), OverrideAttribute) ::
        attr(x.attributes.contains(CompanionAttribute), CompanionAttribute) ::
        Nil)
        .flatten
    sortAttrs(attrs)
  }

  private def sortAttrs(attrs: Seq[Attribute]) =
    attrs.sortBy {
      case PublicAttribute => 1
      case PrivateAttribute => 1
      case ProtectedAttribute => 1
      case OpenAttribute => 2
      case FinalAttribute => 2
      case CaseAttribute => 3
      case DataAttribute => 3
      case OverrideAttribute => 4
      case _ => 5
    }
}

