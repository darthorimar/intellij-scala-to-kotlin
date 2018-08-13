package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.{Exprs, Utils}
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.definition.Definition
import org.jetbrains.plugins.kotlinConverter.scopes.LocalNamer
import org.jetbrains.plugins.kotlinConverter.scopes.ScopedVal.scoped
import org.jetbrains.plugins.kotlinConverter.types.{KotlinTypes, StdTypes}
import org.jetbrains.plugins.kotlinConverter.scopes.ScopedVal.scoped
import org.jetbrains.plugins.scala.lang.dependency.DependencyKind.Reference


object MatchUtils {
  def expandCompositePattern(clauses: Seq[MatchCaseClause]): Seq[MatchCaseClause] =
    clauses.flatMap {
      case MatchCaseClause(CompositePattern(parts, _), expr, guard) =>
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
        case _: LitPattern =>
          Seq.empty
        case ReferencePattern(ref, _) =>
          Seq(ConstructorParam(ValKind, PublicAttribute, ref, NoType))
        case _: WildcardPattern =>
          Seq.empty
        case c: ConstructorPattern =>
          collectVals(c)
        case TypedPattern(ref, exprTypePattern, _) =>
          Seq(ConstructorParam(ValKind, PublicAttribute, ref, exprTypePattern))
      }
    }

    val caseClasses = expandedClauses.collect {
      case MatchCaseClause(pattern@ConstructorPattern(_, _, _, repr), _, _) =>
        val name = Utils.escapeName(s"${repr}_data")
        val vals = collectVals(pattern)
        Defn(Seq(DataAttribute),
          ClassDefn,
          name,
          Seq.empty,
          Some(ParamsConstructor(vals)),
          None,
          None,
          None)
    }

    def collectConstructors(constructors: Seq[(String, CasePattern)]): (Seq[ValOrVarDef], Seq[Expr], Seq[(String, ConstructorPattern)]) = {
      val (vals, conds, refs) = constructors.collect { case (r, ConstructorPattern(constructorRef, patterns, _, _)) =>
        val (destructors, conds, refs) = patterns.map {
          case LitPattern(_, label) =>
            (label.map(ReferencePattern(_, None)).getOrElse(WildcardPattern(None)), None, None)
          case p: ReferencePattern =>
            (p, None, None)
          case p@WildcardPattern(label) =>
            (label.map(ReferencePattern(_, None)).getOrElse(p), None, None)
          case p@ConstructorPattern(CaseClassConstructorRef(name), _, label, _) =>
            val local = label.getOrElse(namerVal.get.newName("l")) //todo use name from pattern
            (ReferencePattern(local, None),
              Some(Exprs.is(LitExpr(NoType, local), ClassType(name))),
              Some(local -> p))
          case p@ConstructorPattern(_: UnapplyCallConstuctorRef, _, label, _) =>
            val local = label.getOrElse(namerVal.get.newName("l")) //todo use name from pattern
            (ReferencePattern(local, None),
              None,
              Some(local -> p))
          case p@TypedPattern(referenceName, _, _) =>
            (ReferencePattern(referenceName, None), Some(Exprs.is(LitExpr(NoType, referenceName), p.patternType)), None)
        }.unzip3

        def rightSide = constructorRef match {
          case CaseClassConstructorRef(_) => Exprs.simpleRef(r, NoType)
          case UnapplyCallConstuctorRef(objectName, unapplyReturnType) =>
            val unapplyRef = RefExpr(NoType, Some(Exprs.simpleRef(objectName, NoType)), "unapply", Seq.empty, true)
            val callExpr = CallExpr(unapplyReturnType, unapplyRef, Seq(Exprs.simpleRef(r, NoType)), Seq.empty)
            Exprs.simpleInfix(unapplyReturnType, "?:", callExpr, ReturnExpr(Some("lazy"), Some(Exprs.nullLit)))
        }

        val valDef = ValOrVarDef(Seq.empty, isVal = true, destructors, Some(rightSide))
        (valDef,
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
      case MatchCaseClause(pattern@ConstructorPattern(constructorRef, _, _, repr), _, guard) =>
        val params = collectVals(pattern).map(v => RefExpr(NoType, None, v.name, Seq.empty, false))
        val callContructor =
          CallExpr(NoType,
            RefExpr(NoType, None, Utils.escapeName(s"${repr}_data"), Seq.empty, true),
            params,
            Seq.empty
          )

        val retExpr = ReturnExpr(Some("lazy"), Some(callContructor))
        val finalExpr = guard match {
          case Some(g) => IfExpr(NoType, g, retExpr, None)
          case None => retExpr
        }

        val refName = constructorRef match {
          case CaseClassConstructorRef(ref) => valRef.referenceName
          case UnapplyCallConstuctorRef(_, _) =>
            namerVal.newName("l")
        }


        val innerBodyExprs =
          handleConstructors(Seq((refName, pattern)), finalExpr)

        val condition = constructorRef match {
          case CaseClassConstructorRef(ref) =>
            if (ref == "Some") Exprs.simpleInfix(StdTypes.BOOLEAN, "!=", valRef, Exprs.nullLit)
            else Exprs.is(valRef, ClassType(ref))
          case UnapplyCallConstuctorRef(_, unapplyReturnType) =>
            val ref = Exprs.simpleRef(refName, unapplyReturnType)
            val notNullExpr = Exprs.simpleInfix(StdTypes.BOOLEAN, "!=", ref, Exprs.nullLit)
            val isExpr = Exprs.is(ref,
              unapplyReturnType match {
                case NullableType(inner) => inner
                case t => t
              })
            Exprs.and(notNullExpr, isExpr)
        }

        val valForUnapplyConstrRef = constructorRef match {
          case UnapplyCallConstuctorRef(objectName, _) =>
            val unapplyRef = RefExpr(NoType, Some(Exprs.simpleRef(objectName, NoType)), "unapply", Seq.empty, true)
            val unapplyCall = CallExpr(NoType, unapplyRef, Seq(valRef), Seq.empty)
            Seq(SimpleValOrVarDef(Seq.empty, true, refName, None, Some(unapplyCall)))
          case _ => Seq.empty
        }

        val body = BlockExpr(NoType,
          valForUnapplyConstrRef ++
            Seq(
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
        case MatchCaseClause(LitPattern(lit, _), e, guard) =>
          val equlasExpr = Exprs.simpleInfix(StdTypes.BOOLEAN, "==", valRef, lit)
          ExprWhenClause(addGuardExpr(equlasExpr, guard), transform[Expr](e))

        case MatchCaseClause(WildcardPattern(_), e, guard) =>
          guard match {
            case Some(g) => ExprWhenClause(transform[Expr](g), transform[Expr](e))
            case None => ElseWhenClause(transform[Expr](e))
          }

        case MatchCaseClause(ReferencePattern(ref, _), e, guard) =>
          scoped(
            renamerVal.updated(_.add(ref -> valRef))
          ) {
            guard match {
              case Some(g) => ExprWhenClause(transform[Expr](g), transform[Expr](e))
              case None => ElseWhenClause(transform[Expr](e))
            }
          }

        case MatchCaseClause(TypedPattern(ref, patternTy, _), e, guard) =>
          scoped(
            renamerVal.updated(_.add(ref -> valRef))
          ) {
            ExprWhenClause(addGuardExpr(Exprs.is(valRef, patternTy), guard.map(transform[Expr])), transform[Expr](e))
          }

        case MatchCaseClause(pattern@ConstructorPattern(_, _, _, repr), e, _) =>
          val lazyRef = RefExpr(NoType, None, Utils.escapeName(repr), Seq.empty, false)
          val notEqulasExpr = Exprs.simpleInfix(StdTypes.BOOLEAN, "!=", lazyRef, Exprs.nullLit)
          val vals = collectVals(pattern)
          val valDef = ValOrVarDef(Seq.empty, true, vals.map(p => ReferencePattern(p.name, None)), Some(lazyRef))
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
    val elseClause = if (!whenClauses.exists {
      case _: ElseWhenClause => true
      case _ => false
    }) {
      addDefinition(Definition.matchError)
      val exception = NewExpr(ClassType("MatchError"), Seq(valRef))
      Seq(ElseWhenClause(ThrowExpr(exception)))
    }
    else Seq.empty

    val whenExpr = WhenExpr(NoType, None, whenClauses ++ elseClause)
    (caseClasses ++ lazyDefs) :+ whenExpr
  }

}
