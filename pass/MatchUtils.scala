package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.{Exprs, Utils}
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.scopes.LocalNamer
import org.jetbrains.plugins.kotlinConverter.scopes.ScopedVal.scoped
import org.jetbrains.plugins.kotlinConverter.types.KotlinTypes
import org.jetbrains.plugins.kotlinConverter.scopes.ScopedVal.scoped


object MatchUtils {
  def expandCompositePattern(clauses: Seq[MatchCaseClause]): Seq[MatchCaseClause] =
    clauses.flatMap {
      case MatchCaseClause(CompositePattern(parts), expr, guard) =>
        parts.map { p =>
          MatchCaseClause(p, expr, guard)
        }
      case x => Seq(x)
    }

  def convertMatchToWhen(valRef: RefExpr,
                         clauses: Seq[MatchCaseClause],
                         exprType: Type,
                         transformInst: Transform): Seq[Expr] = {
    import transformInst._

    val expandedClauses = MatchUtils.expandCompositePattern(clauses)

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
            if (ref == "Some") Exprs.simpleInfix(KotlinTypes.BOOLEAN, "!=", LitExpr(exprType, local), Exprs.nullLit)
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
          if (ref == "Some") Exprs.simpleInfix(KotlinTypes.BOOLEAN, "!=", valRef, Exprs.nullLit)
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
          val equlasExpr = Exprs.simpleInfix(KotlinTypes.BOOLEAN, "==", valRef, lit)
          ExprWhenClause(addGuardExpr(equlasExpr, guard), transform[Expr](e))

        case MatchCaseClause(WildcardPattern, e, guard) =>
          guard match {
            case Some(g) => ExprWhenClause(transform[Expr](g), transform[Expr](e))
            case None => ElseWhenClause(transform[Expr](e))
          }

        case MatchCaseClause(ReferencePattern(ref), e, guard) =>
          scoped(
            renamesVal.updated(_.add(ref -> valRef.referenceName))
          ) {
            guard match {
              case Some(g) => ExprWhenClause(transform[Expr](g), transform[Expr](e))
              case None => ElseWhenClause(transform[Expr](e))
            }
          }

        case MatchCaseClause(TypedPattern(ref, patternTy), e, guard) =>
          scoped(
            renamesVal.updated(_.add(ref -> valRef.referenceName))
          ) {
            ExprWhenClause(addGuardExpr(Exprs.is(valRef, patternTy), guard.map(transform[Expr])), transform[Expr](e))
          }

        case MatchCaseClause(pattern@ConstructorPattern(ref, args, _, repr), e, _) =>
          val lazyRef = RefExpr(NoType, None, Utils.escapeName(repr), Seq.empty, false)
          val notEqulasExpr = Exprs.simpleInfix(KotlinTypes.BOOLEAN, "!=", lazyRef, Exprs.nullLit)
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
        .span(_.isInstanceOf[ExprWhenClause]) match { //take all before first `else` including it
        case (h, t) => h ++ t.headOption.toSeq
      }

    val whenExpr = WhenExpr(NoType, None, whenClauses)
    (caseClasses ++ lazyDefs) :+ whenExpr
  }

}
