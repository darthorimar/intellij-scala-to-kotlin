package org.jetbrains.plugins.kotlinConverter.pass

import com.intellij.formatting.BlockEx
import com.sun.source.doctree.AttributeTree.ValueKind
import org.jetbrains.plugins.kotlinConverter
import org.jetbrains.plugins.kotlinConverter.{Exprs, Utils}
import org.jetbrains.plugins.kotlinConverter.types.KotlinTypes
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.scopes.ScopedVal.scoped
import org.jetbrains.plugins.kotlinConverter.scopes.{LocalNamer, Renames, ScopedVal}


class BasicTransform extends Transform {
  val renamesVal = new ScopedVal[Renames](Renames(Map.empty))
  val namerVal = new ScopedVal[LocalNamer](new LocalNamer)

  override protected def action(ast: AST): Option[AST] = {
    ast match {


      //import _ --> *
      case ImportDef(ref, names) =>
        Some(ImportDef(ref, names.map {
          case "_" => "*"
          case x => x
        }))

      //rename refs
      case x: RefExpr if renamesVal.call(_.renames.contains(x.referenceName)) =>
        Some(x.copy(referenceName = renamesVal.get.renames(x.referenceName)))

      //Remove renault brackets for lambda like in seq.map {x => x * 2}
      case BlockExpr(_, stmts) if stmts.size == 1 && stmts.head.isInstanceOf[LambdaExpr] =>
        Some(pass[Expr](stmts.head))

      case ParamsConstructor(params)
        if parent.asInstanceOf[Defn].attributes.contains(CaseAttribute) =>
        Some(ParamsConstructor(params.map {
          case ConstructorParam(parType, mod, name, exprType) =>
            val t = if (parType == NoMemberKind) ValKind else parType
            val m = if (mod == NoAttribute) PublicAttribute else mod
            ConstructorParam(t, m, name, pass[Type](exprType))
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


        val result = generators.reverse.foldLeft(pass[Expr](yieldedBody): Expr) {
          case (acc, ForGenerator(pattern, expr)) =>
            ForInExpr(NoType, RefExpr(NoType, None, pattern.name, Seq.empty, false), pass[Expr](expr), wrapToBody(acc))
          case (acc, ForGuard(condition)) =>
            IfExpr(NoType, pass[Expr](condition), wrapToBody(acc), None)
          case (acc, ForVal(valDefExpr)) =>
            BlockExpr(NoType, Seq(
              pass[Expr](valDefExpr),
              acc))
        }
        if (isYield) {
          collectedImports = ImportDef("kotlin.coroutines.experimental.buildSequence", Seq.empty) :: collectedImports
          Some(
            Exprs.simpleCall("buildSequence",
              exprType,
              Seq(LambdaExpr(exprType, Seq.empty, result, false))))
        } else Some(result)


      // sort fun attrs, add return to the function end
      case x: DefnDef =>
        scoped(
          namerVal.set(new LocalNamer)
        ) {
          val newDef = copy(x).asInstanceOf[DefnDef]

          def handleBody(body: Expr) = body match {
            case BlockExpr(exprType, stmts) =>
              BlockExpr(exprType, stmts.init :+ ReturnExpr(None, Some(stmts.last)))
            case b => b
          }

          Some(newDef.copy(attributes = handleAttrs(x), body = newDef.body.map(handleBody)))
        }

      case x: ValOrVarDef =>
        Some(copy(x).asInstanceOf[ValOrVarDef].copy(attributes = handleAttrs(x)))

      case x: SimpleValOrVarDef =>
        Some(copy(x).asInstanceOf[SimpleValOrVarDef].copy(attributes = handleAttrs(x)))

      case x: Defn =>
        val defn = copy(x).asInstanceOf[Defn]
        val t =
          if (x.defnType == TraitDefn) InterfaceDefn
          else x.defnType
        Some(copy(defn).asInstanceOf[Defn].copy(attributes = handleAttrs(defn), defnType = t))

      //uncarry
      case x@CallExpr(_, c: CallExpr, _) if c.exprType.isFunction =>
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
          pass[Type](x.exprType),
          copy(ref).asInstanceOf[RefExpr],
          params.map(pass[Expr])))


      //a.foo(f) --> a.foo{f(it)}
      case CallExpr(exprType, ref, params)
        if params.exists {
          case y: RefExpr if y.isFunctionRef => true
          case _ => false
        } =>

        Some(
          CallExpr(
            pass[Type](exprType),
            pass[Expr](ref),
            params.map {
              case y: RefExpr if y.isFunctionRef =>
                LambdaExpr(
                  exprType,
                  Seq.empty,
                  CallExpr(pass[Type](y.exprType), copy(y).asInstanceOf[Expr], Seq(UnderscoreExpr(y.exprType))),
                  false)
              case y => y
            }))

      //x.foo --> x.foo()
      case x@RefExpr(exprType, obj, ref, exprTypepeParams, true)
        if !parent.isInstanceOf[CallExpr] =>
        Some(CallExpr(exprType, copy(x).asInstanceOf[RefExpr], Seq.empty))

      // matchExpr to when one
      case MatchExpr(exprType, expr, clauses) =>
        val expandedClauses = clauses.flatMap {
          case MatchCaseClause(CompositePattern(parts), expr, guard) =>
            parts.map { p =>
              MatchCaseClause(p, expr, guard)
            }
          case x => Seq(x)
        }

        val newExpr = pass[Expr](expr)
        val valExpr = SimpleValOrVarDef(Seq.empty, true, namerVal.newName("match"), None, Some(newExpr))
        val valRef = RefExpr(newExpr.exprType, None, valExpr.name, Seq.empty, false)

        def collectVals(constructorPatternMatch: ConstructorPattern): Seq[ConstructorParam] = {
          constructorPatternMatch.args.flatMap {
            case LitPattern(litPattern) =>
              Seq.empty
            case ReferencePattern(ref) =>
              Seq(ConstructorParam(ValKind, PublicAttribute, ref, NoType))
            case WildcardPattern =>
              Seq.empty
            case c: ConstructorPattern =>
              collectVals(c)
            case TypedPattern(ref, exprTypePattern) =>
              Seq(ConstructorParam(ValKind, PublicAttribute, ref, exprTypePattern))
          }
        }

        val caseClasses = expandedClauses.collect {
          case MatchCaseClause(pattern@ConstructorPattern(_, _, _, repr), _, _) =>
            val name = Utils.escapeName(s"${repr}_data")
            val vals = collectVals(pattern)
            Defn(Seq(CaseAttribute),
              ClassDefn,
              name,
              Seq.empty,
              Some(ParamsConstructor(vals)),
              None,
              None)
        }

        def collectConstructors(constructors: Seq[(String, CasePattern)]): (Seq[ValOrVarDef], Seq[Expr], Seq[(String, ConstructorPattern)]) = {
          val (vals, conds, refs) = constructors.collect { case (r, ConstructorPattern(_, patterns, _, _)) =>
            val (destructors, conds, refs) = patterns.map {
              case LitPattern(litPattern) =>
                (LitPattern(litPattern), None, None)
              case ReferencePattern(ref) =>
                (ReferencePattern(ref), None, None)
              case WildcardPattern =>
                (WildcardPattern, None, None)
              case c@ConstructorPattern(ref, _, label, _) =>
                val local = label.getOrElse(namerVal.get.newName("l")) //todo use name from pattern
              val condition =
                if (ref == "Some") BinExpr(KotlinTypes.BOOLEAN, "!=", LitExpr(exprType, local), Exprs.nullLit)
                else Exprs.is(LitExpr(exprType, local), SimpleType(ref))
                (ReferencePattern(local),
                  Some(condition),
                  Some(local -> c))
              case TypedPattern(ref, exprTypePattern) =>
                (ReferencePattern(ref),
                  Some(Exprs.is(LitExpr(exprTypePattern, ref), exprTypePattern)),
                  None)
            }.unzip3
            (ValOrVarDef(Seq.empty, true, destructors, Some(RefExpr(NoType, None, r, Seq.empty, false))),
              conds.flatten,
              refs.flatten)
          }.unzip3
          (vals, conds.flatten, refs.flatten)
        }

        def handleConstructors(constructors: Seq[(String, CasePattern)], defaultCase: Expr): Seq[Expr] = {
          val (valDefns, conditionParts, collectedConstructors) = collectConstructors(constructors)

          val trueBlock =
            if (collectedConstructors.nonEmpty) {
              val exprs = handleConstructors(collectedConstructors, defaultCase)
              BlockExpr(exprs.last.exprType, exprs)
            } else defaultCase

          val ifCond =
            if (conditionParts.nonEmpty)
              IfExpr(NoType, conditionParts.reduceLeft(Exprs.and), trueBlock, None)
            else trueBlock

          valDefns :+ ifCond
        }

        val lazyDefs = expandedClauses.collect {
          case MatchCaseClause(pattern@ConstructorPattern(ref, _, _, repr), expr, guard) =>
            val params = collectVals(pattern).map(v => RefExpr(NoType, None, v.name, Seq.empty, false))
            val callContructor =
              CallExpr(NoType,
                RefExpr(NoType, None, Utils.escapeName(s"${repr}_data"), Seq.empty, true),
                params
              )

            val retExpr = ReturnExpr(Some("lazy"), Some(callContructor))
            val finalExpr = guard match {
              case Some(g) => IfExpr(NoType, g, retExpr, None)
              case None => retExpr
            }

            val innerBodyExprs =
              handleConstructors(Seq((valRef.referenceName, pattern)), finalExpr)

            val condition =
              if (ref == "Some") BinExpr(KotlinTypes.BOOLEAN, "!=", valRef, Exprs.nullLit)
              else Exprs.is(valRef, SimpleType(ref))

            val body = BlockExpr(NoType, Seq(
              IfExpr(
                NoType,
                condition,
                BlockExpr(innerBodyExprs.last.exprType, innerBodyExprs),
                None),
              ReturnExpr(Some("lazy"), Some(Exprs.nullLit))
            ))
            LazyValDef(Utils.escapeName(repr), NoType, body)
        }

        def addGuardExpr(expr: Expr, guard: Option[Expr]) =
          guard match {
            case Some(g) => Exprs.and(expr, g)
            case None => expr
          }

        val whenClauses =
          expandedClauses.map {
            case MatchCaseClause(LitPattern(lit), e, guard) =>
              val equlasExpr = BinExpr(KotlinTypes.BOOLEAN, "==", valRef, lit)
              ExprWhenClause(addGuardExpr(equlasExpr, guard), pass[Expr](e))

            case MatchCaseClause(WildcardPattern, e, guard) =>
              guard match {
                case Some(g) => ExprWhenClause(pass[Expr](g), pass[Expr](e))
                case None => ElseWhenClause(pass[Expr](e))
              }

            case MatchCaseClause(ReferencePattern(ref), e, guard) =>
              scoped(
                renamesVal.updated(_.add(ref -> valRef.referenceName))
              ) {
                guard match {
                  case Some(g) => ExprWhenClause(pass[Expr](g), pass[Expr](e))
                  case None => ElseWhenClause(pass[Expr](e))
                }
              }

            case MatchCaseClause(TypedPattern(ref, patternTy), e, guard) =>
              scoped(
                renamesVal.updated(_.add(ref -> valExpr.name))
              ) {
                ExprWhenClause(addGuardExpr(Exprs.is(valRef, patternTy), guard.map(pass[Expr])), pass[Expr](e))
              }

            case MatchCaseClause(pattern@ConstructorPattern(ref, args, _, repr), e, _) =>
              val lazyRef = RefExpr(NoType, None, Utils.escapeName(repr), Seq.empty, false)
              val notEqulasExpr = BinExpr(KotlinTypes.BOOLEAN, "!=", lazyRef, Exprs.nullLit)
              val vals = collectVals(pattern)
              val valDef = ValOrVarDef(Seq.empty, true, vals.map(p => ReferencePattern(p.name)), Some(lazyRef))
              val body = e match {
                case BlockExpr(exprType, exprs) =>
                  BlockExpr(exprType, valDef +: exprs)
                case expr =>
                  BlockExpr(expr.exprType, Seq(valDef, expr))
              }
              ExprWhenClause(notEqulasExpr, body)
          }
            .span(_.isInstanceOf[ExprWhenClause]) match { //take all before first else including first else
            case (h, t) => h ++ t.headOption.toSeq
          }

        val whenExpr = WhenExpr(NoType, None, whenClauses)
        Some(pass[Expr](BlockExpr(whenExpr.exprType, valExpr +: (caseClasses ++ lazyDefs) :+ whenExpr)))

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


    val attrs =
      (attr(x.attributes.contains(CaseAttribute) && x.isClassDefn, DataAttribute) ::
        attr(isOpen, OpenAttribute) ::
        //        attr(x.attrs.contains(PublicAttr), PublicAttr) ::
        attr(x.attributes.contains(PrivateAttribute), PrivateAttribute) ::
        attr(x.attributes.contains(ProtectedAttribute), ProtectedAttribute) ::
        attr(x.attributes.contains(AbstractAttribute), AbstractAttribute) ::
        attr(x.attributes.contains(OverrideAttribute), OverrideAttribute) ::
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

