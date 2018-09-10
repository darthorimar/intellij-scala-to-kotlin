package darthorimar.scalaToKotlinConverter.step.transform

import darthorimar.scalaToKotlinConverter.{ Exprs, Utils, ast }
import darthorimar.scalaToKotlinConverter.ast._
import darthorimar.scalaToKotlinConverter.scopes.ScopedVal.scoped
import darthorimar.scalaToKotlinConverter.scopes.{ BasicTransformState, LocalNamer, ScopedVal }
import darthorimar.scalaToKotlinConverter.step.transform.Helpers.ApplyCall.{ InfixCall, PrefixCall }
import darthorimar.scalaToKotlinConverter.step.transform.MatchUtils.generateInitialisationExprByConstructorPattern
import darthorimar.scalaToKotlinConverter.types.TypeUtils.NumericType
import darthorimar.scalaToKotlinConverter.types.{ KotlinTypes, StdTypes }

class BasicTransform extends Transform {
  override def name: String = "Converting Kotlin Code"

  val stateVal: ScopedVal[BasicTransformState] = new ScopedVal[BasicTransformState](BasicTransformState(false))

  private def isDefaultInfix(name: String, leftType: Type, rightType: Type, returnType: Type): Boolean =
    (name, leftType, rightType, returnType) match {
      case ("||" | "&&", StdTypes.BOOLEAN, StdTypes.BOOLEAN, _)             => true
      case ("*" | "/" | "+" | "-" | "%", NumericType(_), NumericType(_), _) => true
      case (">" | ">=" | "<" | "<=", NumericType(_), NumericType(_), _)     => true
      case ("==" | "!=", _, _, _)                                           => true
      case ("+" | "++", StdTypes.STRING, _, _)                              => true
      case _                                                                => false
    }

  private def isDefaultPrefix(name: String, argType: Type, returnType: Type): Boolean =
    (name, argType, returnType) match {
      case ("!", StdTypes.BOOLEAN, StdTypes.BOOLEAN) => true
      case _                                         => false
    }

  override protected val action: PartialFunction[AST, AST] = {
    case InfixCall(name, left, right, returnType) if isDefaultInfix(name, left.exprType, right.exprType, returnType) =>
      Exprs.simpleInfix(returnType, name, transform[Expr](left), transform[Expr](right))

    case PrefixCall(name, arg, returnType) if isDefaultPrefix(name, arg.exprType, returnType) =>
      PrefixExpr(returnType, transform[Expr](arg), name)

    //scala try --> kotlin try
    case ScalaTryExpr(exprType, tryBlock, catchBlock, finallyBlock) =>
      val cases = MatchUtils.expandCompositePatternAndApplyTransform(catchBlock.toSeq.flatMap(_.cases), this)
      val (goodClauses, badClauses) = cases.span {
        case MatchCaseClause(_: TypedPattern, _, None)     => true
        case MatchCaseClause(_: ReferencePattern, _, None) => true
        case _                                             => false
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
          val ref       = Exprs.simpleRef(namerVal.newName("e"), KotlinTypes.THROWABLE)
          val matchExpr = BlockExpr(MatchUtils.convertMatchToWhen(ref, badClauses, exprType, this))
          Seq(KotlinCatchCase(ref.referenceName, ref.exprType, matchExpr))
      }
      KotlinTryExpr(exprType,
                    transform[Expr](tryBlock),
                    (goodCatches ++ badCatches).map(transform[KotlinCatchCase]),
                    finallyBlock.map(transform[Expr]))

    //rename refs
    case x: RefExpr if renamerVal.call(_.renames.contains(x.referenceName)) =>
      renamerVal.get.renames(x.referenceName)

    //Remove renault brackets for lambda like in seq.map {x => x * 2}
    case BlockExpr(stmts) if stmts.size == 1 && stmts.head.isInstanceOf[LambdaExpr] =>
      transform[Expr](stmts.head)

    case ParamsConstructor(params) =>
      ParamsConstructor(params.map {
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
      })

    case ForExpr(exprType, generators, isYield, body) =>
      def wrapToBody(expr: Expr) = expr match {
        case x: BlockExpr => x
        case _            => BlockExpr(Seq(expr))
      }

      val yieldedBody =
        if (isYield)
          Exprs.simpleCall("yield", exprType, Seq(body))
        else body

      val result = generators.reverse.foldLeft(transform[Expr](yieldedBody): Expr) {
        case (acc, ForGenerator(pattern, expr)) =>
          ForInExpr(NoType,
                    RefExpr(NoType, None, pattern.representation, Seq.empty, false),
                    transform[Expr](expr),
                    wrapToBody(acc))
        case (acc, ForGuard(condition)) =>
          IfExpr(NoType, transform[Expr](condition), wrapToBody(acc), None)
        case (acc, ForVal(valDefExpr)) =>
          BlockExpr(Seq(transform[Expr](valDefExpr), acc))
      }
      if (isYield) {
        stateStepVal.addImport(Import("kotlin.coroutines.experimental.buildSequence"))
        Exprs.simpleCall("buildSequence", exprType, Seq(LambdaExpr(exprType, Seq.empty, result, false)))
      } else result

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
        val newDef = copy[DefnDef](x)

        def handleBody(body: Expr) = body match {
          case b @ BlockExpr(stmts) =>
            val last = stmts.last
            if (!last.isInstanceOf[ReturnExpr] && x.returnType != StdTypes.UNIT)
              BlockExpr(stmts.init :+ ReturnExpr(None, Some(last)))
            else b
          case b => b
        }

        val params = newDef.parameters.map {
          case DefParameter(parameterType, name, isVarArg, true) =>
            DefParameter(FunctionType(StdTypes.UNIT, parameterType), name, isVarArg, true)
          case p => p
        }

        newDef.copy(attributes = handleAttrs(newDef), body = newDef.body.map(handleBody), parameters = params)
      }

    // Scala val or var def --> Kotlin val or var def right part is not nested constructor call
    case valOrVarDef @ ScalaValOrVarDef(attributes, isVal, constructorPattern, expr)
        if constructorPattern.patterns.forall(!_.isConstructorPattern) =>
      val valDef = MatchUtils.createConstructorValOrVarDefinition(constructorPattern, isVal, expr)
      valDef.copy(attributes = handleAttrs(valOrVarDef))

    // Scala val or var def --> Kotlin val or var def right part has nested constructor calls
    case valOrVarDef @ ScalaValOrVarDef(attributes, isVal, constructorPattern, expr) =>
      val (refValDef, valRef) = expr match {
        case refExpr: RefExpr => (None, refExpr)
        case _ =>
          val local = namerVal.newName("l")
          (Some(SimpleValOrVarDef(Seq.empty, isVal = true, local, None, Some(expr))),
           Exprs.simpleRef(local, expr.exprType))
      }

      val dataClass = MatchUtils.generateDataClassByConstructorPattern(constructorPattern)
      val generateSuccessExpr = (callConstructor: CallExpr) => {
        ReturnExpr(Some("run"), Some(callConstructor))
      }
      val errorExpr = ThrowExpr(NewExpr(ClassType("MatchError"), Seq(valRef)))
      val body =
        generateInitialisationExprByConstructorPattern(constructorPattern, valRef, generateSuccessExpr, errorExpr, this)
      val valDef = MatchUtils.createConstructorValOrVarDefinition(constructorPattern, isVal, body)
      ExprContainer(refValDef.toSeq :+ dataClass :+ valDef.copy(attributes = handleAttrs(valOrVarDef)))

    case x: SimpleValOrVarDef =>
      copy[SimpleValOrVarDef](x).copy(attributes = handleAttrs(x))

    //implicit class --> extension function
    case Defn(attrs,
              ClassDefn,
              _,
              typeParams,
              Some(ParamsConstructor(Seq(ConstructorParam(_, _, parameterName, parameterType)))),
              _,
              Some(BlockExpr(defns)),
              _) if attrs.contains(ImplicitAttribute) =>
      val functions = defns collect {
        case defn: DefnDef =>
          scoped(
            renamerVal.updated(_.add(parameterName -> ThisExpr(parameterType)))
          ) {
            val transformed = transform[DefnDef](defn.copy(typeParameters = typeParams ++ defn.typeParameters))
            transformed.copy(receiver = Some(parameterType))
          }
      }
      ExprContainer(functions)

    case x: Defn =>
      scoped(
        stateVal.updated { s =>
          BasicTransformState(s.inCompanionObject || x.isObjectDefn && x.defnType == ObjDefn)
        }
      ) {

        val defn = copy[Defn](x)

        def generateFakeCompanion: Defn = {
          val defnType = ClassType(defn.name)
          val applyDef = {
            val parameters = defn.constructor
              .collect {
                case ParamsConstructor(ps) => ps
              }
              .toSeq
              .flatten
              .map {
                case ConstructorParam(_, _, paramName, parameterType) =>
                  DefParameter(parameterType, paramName, false, false)
              }
            val arguments =
              parameters.map {
                case DefParameter(parameterType, paramName, _, _) =>
                  Exprs.simpleRef(paramName, parameterType)
              }
            val body = NewExpr(defnType, arguments)
            DefnDef(Seq.empty, receiver = None, "apply", Seq.empty, parameters, defnType, Some(body))
          }
          val unapplyDef = {
            val parameters =
              Seq(DefParameter(defnType, "x", false, false))
            val body = Exprs.simpleRef("x", defnType)
            DefnDef(Seq.empty, receiver = None, "unapply", Seq.empty, parameters, NullableType(defnType), Some(body))
          }
          Defn(Seq(CompanionAttribute),
               ObjDefn,
               "",
               Seq.empty,
               None,
               None,
               Some(BlockExpr(Seq(applyDef, unapplyDef))),
               None)
        }

        val companionObj =
          defn.companionDefn
            .flatMap {
              case ClassCompanion(c) =>
                Some(c.copy(attributes = c.attributes :+ CompanionAttribute))
              case _ => None
            }
            .orElse {
              if (x.attributes.contains(CaseAttribute) | x.attributes.contains(DataAttribute)) {
                Some(generateFakeCompanion)
              } else None
            }
            .toSeq
        val exprs = defn.body.toSeq.flatMap(_.exprs) ++ companionObj
        val newBody =
          if (exprs.isEmpty) None
          else Some(BlockExpr(exprs))

        val defnType =
          if (defn.defnType == TraitDefn) InterfaceDefn
          else defn.defnType

        val name =
          if (defn.companionDefn.contains(ObjectCompanion)) ""
          else defn.name

        copy[Defn](defn)
          .copy(attributes = handleAttrs(defn), defnType = defnType, body = newBody, name = name)
      }

    //uncarry
    case x @ CallExpr(_, c: CallExpr, _, _) if c.exprType.isFunction =>
      def collectParams(c: Expr): List[Expr] = c match {
        case x: CallExpr if x.ref.exprType.isFunction =>
          collectParams(x.ref) ++ x.params.toList
        case _ => Nil
      }

      def collectRef(c: CallExpr): Expr = c.ref match {
        case x: CallExpr => collectRef(x)
        case x           => x
      }

      val params = collectParams(x)
      val ref    = collectRef(x)
      CallExpr(transform[Type](x.exprType), copy[RefExpr](ref), params.map(transform[Expr]), Seq.empty)

    //a.foo(f) --> a.foo{f(it)}
    //a.foo(_ + 1) --> a.foo {it + 1}
    //handle call by name params
    case CallExpr(exprType, ref, params, paramsExpectedTypes) =>
      val paramsInfo =
        paramsExpectedTypes ++ Seq.fill(params.length - paramsExpectedTypes.length)(CallParameterInfo(NoType, false))

      CallExpr(
        transform[Type](exprType),
        transform[Expr](ref),
        params
          .zip(paramsInfo)
          .map {
            case (y: RefExpr, CallParameterInfo(_: FunctionType, _)) if y.isFunctionRef =>
              LambdaExpr(
                exprType,
                Seq.empty,
                CallExpr(transform[Type](y.exprType), transform[Expr](y), Seq(UnderscoreExpr(y.exprType)), Seq.empty),
                false)
            case (y @ RefExpr(exprType, obj, ref, typeParams, true), _) =>
              CallExpr(exprType, transform[Expr](y), Seq.empty, Seq.empty)
            case (y, _) => transform[Expr](y)
          }
          .zip(paramsInfo)
          .map {
            case (e, CallParameterInfo(_, true)) =>
              val exp = transform[Expr](e)
              LambdaExpr(exp.exprType, Seq.empty, exp, needBraces = false)
            case (e, _) => e
          },
        Seq.empty
      )

    //x.foo --> x.foo()
    case x @ RefExpr(exprType, obj, ref, typeParams, true) if !parent.isInstanceOf[CallExpr] =>
      CallExpr(exprType, copy[RefExpr](x), Seq.empty, Seq.empty)

    //foo.apply(sth) --> foo(sth)
    case x @ RefExpr(exprType, Some(obj), "apply", typeParams, _)
        if parent.isInstanceOf[CallExpr] && obj.exprType.isFunction =>
      transform[Expr](obj)

    // matchExpr to when one
    case MatchExpr(exprType, expr, clauses) =>
      val newExpr  = transform[Expr](expr)
      val valExpr  = SimpleValOrVarDef(Seq.empty, true, namerVal.newName("match"), None, Some(newExpr))
      val valRef   = RefExpr(newExpr.exprType, None, valExpr.name, Seq.empty, false)
      val whenExpr = MatchUtils.convertMatchToWhen(valRef, clauses, exprType, this)
      BlockExpr(valExpr +: whenExpr)

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
        Nil).flatten
    sortAttrs(attrs)
  }

  private def sortAttrs(attrs: Seq[Attribute]) =
    attrs.sortBy {
      case PublicAttribute    => 1
      case PrivateAttribute   => 1
      case ProtectedAttribute => 1
      case OpenAttribute      => 2
      case FinalAttribute     => 2
      case CaseAttribute      => 3
      case DataAttribute      => 3
      case OverrideAttribute  => 4
      case _                  => 5
    }
}
