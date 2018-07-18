package org.jetbrains.plugins.kotlinConverter.pass

import com.intellij.formatting.BlockEx
import com.sun.source.doctree.AttributeTree.ValueKind
import org.jetbrains.plugins.kotlinConverter
import org.jetbrains.plugins.kotlinConverter.{Exprs, Utils}
import org.jetbrains.plugins.kotlinConverter.types.KotlinTypes
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.scopes.ScopedVal.scoped
import org.jetbrains.plugins.kotlinConverter.scopes.{LocalNamer, Renames, ScopedVal}


class BasicPass extends Pass {
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
      case x: RefExpr if renamesVal.call(_.renames.contains(x.ref)) =>
        Some(x.copy(ref = renamesVal.get.renames(x.ref)))

      //Remove renault brackets for lambda like in seq.map {x => x * 2}
      case BlockExpr(_, stmts) if stmts.size == 1 && stmts.head.isInstanceOf[LambdaExpr] =>
        Some(pass[Expr](stmts.head))

      case ParamsConstruct(params)
        if parent.asInstanceOf[Defn].attrs.contains(CaseAttr) =>
        Some(ParamsConstruct(params.map {
          case ConstructParam(parType, mod, name, ty) =>
            val t = if (parType == NoMemberKind) ValKind else parType
            val m = if (mod == NoAttr) PublAttr else mod
            ConstructParam(t, m, name, pass[Type](ty))
        }))

      //      sort fun attrs, add return to the funcion end
      case x: DefnDef =>
        scoped(
          namerVal.set(new LocalNamer)
        ) {
          val newDef = copy(x).asInstanceOf[DefnDef]

          def handleBody(body: Expr) = body match {
            case BlockExpr(ty, stmts) =>
              BlockExpr(ty, stmts.init :+ ReturnExpr(None, Some(stmts.last)))
            case b => b
          }

          Some(newDef.copy(attrs = sortAttrs(x.attrs), body = newDef.body.map(handleBody)))
        }

      case x: Defn =>
        val defn = copy(x).asInstanceOf[Defn]
        val t =
          if (x.t == TraitDefn) InterfaceDefn
          else x.t
        Some(copy(defn).asInstanceOf[Defn].copy(attrs = handleAttrs(defn), t = t))

      //uncarry
      case x@CallExpr(_, c: CallExpr, _) if c.ty.isFunc =>
        def collectParams(c: Expr): List[Expr] = c match {
          case x: CallExpr if x.ref.ty.isFunc =>
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
          pass[Type](x.ty),
          copy(ref).asInstanceOf[RefExpr],
          params.map(pass[Expr])))


      //a.foo(f) --> a.foo{f(it)}
      case CallExpr(ty, ref, params)
        if params.exists {
          case y: RefExpr if y.isFunc => true
          case _ => false
        } =>

        Some(
          CallExpr(
            pass[Type](ty),
            pass[Expr](ref),
            params.map {
              case y: RefExpr if y.isFunc =>
                LambdaExpr(
                  ty,
                  Seq.empty,
                  CallExpr(pass[Type](y.ty), copy(y).asInstanceOf[Expr], Seq(UnderScExpr(y.ty))),
                  false)
              case y => y
            }))

      //x.foo --> x.foo()
      case x@RefExpr(ty, obj, ref, typeParams, true)
        if !parent.isInstanceOf[CallExpr] =>
        Some(CallExpr(ty, copy(x).asInstanceOf[RefExpr], Seq.empty))

      // matchExpr to when one
      case MatchExpr(ty, expr, clauses) =>
        val expandedClauses = clauses.flatMap {
          case MatchCaseClause(CompositePatternMatch(parts), expr, guard) =>
            parts.map { p =>
              MatchCaseClause(p, expr, guard)
            }
          case x => Seq(x)
        }

        val newExpr = pass[Expr](expr)
        val valExpr = ValDef(Seq(ReferencePatternMatch(namerVal.get.newName("match"))), newExpr)
        val valRef = RefExpr(newExpr.ty, None, valExpr.destructors.head.name, Seq.empty, false)

        def collectVals(constructorPatternMatch: ConstructorPatternMatch): Seq[ConstructParam] = {
          constructorPatternMatch.args.flatMap {
            case LitPatternMatch(litPattern) =>
              Seq.empty
            case ReferencePatternMatch(ref) =>
              Seq(ConstructParam(ValKind, PublAttr, ref, NoType))
            case WildcardPatternMatch =>
              Seq.empty
            case c: ConstructorPatternMatch =>
              collectVals(c)
            case TypedPatternMatch(ref, tyPattern) =>
              Seq(ConstructParam(ValKind, PublAttr, ref, tyPattern))
          }
        }

        val caseClasses = expandedClauses.collect {
          case MatchCaseClause(pattern@ConstructorPatternMatch(_, _, _, repr), _, _) =>
            val name = Utils.escapeName(s"${repr}_data")
            val vals = collectVals(pattern)
            Defn(Seq(CaseAttr),
              ClassDefn,
              name,
              Seq.empty,
              Some(ParamsConstruct(vals)),
              Seq.empty,
              None)
        }

        def collectConstructors(constructors: Seq[(String, MatchCasePattern)]): (Seq[ValDef], Seq[Expr], Seq[(String, ConstructorPatternMatch)]) = {
          val (vals, conds, refs) = constructors.collect { case (r, ConstructorPatternMatch(_, patterns, _, _)) =>
            val (destructors, conds, refs) = patterns.map {
              case LitPatternMatch(litPattern) =>
                (LitPatternMatch(litPattern), None, None)
              case ReferencePatternMatch(ref) =>
                (ReferencePatternMatch(ref), None, None)
              case WildcardPatternMatch =>
                (WildcardPatternMatch, None, None)
              case c@ConstructorPatternMatch(ref, _, label, _) =>
                val local = label.getOrElse(namerVal.get.newName("l")) //todo use name from pattern
                println(ref)
                val condition =
                  if (ref == "Some") BinExpr(KotlinTypes.BOOLEAN, "!=", LitExpr(ty, local), Exprs.nullLit)
                  else Exprs.is(LitExpr(ty, local), SimpleType(ref))
                (ReferencePatternMatch(local),
                  Some(condition),
                  Some(local -> c))
              case TypedPatternMatch(ref, tyPattern) =>
                (ReferencePatternMatch(ref),
                  Some(Exprs.is(LitExpr(tyPattern, ref), tyPattern)),
                  None)
            }.unzip3
            (ValDef(destructors, RefExpr(NoType, None, r, Seq.empty, false)),
              conds.flatten,
              refs.flatten)
          }.unzip3
          (vals, conds.flatten, refs.flatten)
        }

        def handleConstructors(constructors: Seq[(String, MatchCasePattern)], defaultCase: Expr): Seq[Expr] = {
          val (valDefns, conditionParts, collectedConstructors) = collectConstructors(constructors)

          val trueBlock =
            if (collectedConstructors.nonEmpty) {
              val exprs = handleConstructors(collectedConstructors, defaultCase)
              BlockExpr(exprs.last.ty, exprs)
            } else defaultCase

          val ifCond =
            if (conditionParts.nonEmpty)
              IfExpr(NoType, conditionParts.reduceLeft(Exprs.and), trueBlock, None)
            else trueBlock

          valDefns :+ ifCond
        }

        val lazyDefs = expandedClauses.collect {
          case MatchCaseClause(pattern@ConstructorPatternMatch(ref, _, _, repr), expr, guard) =>
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
              handleConstructors(Seq((valRef.ref, pattern)), finalExpr)

            val condition =
              if (ref == "Some") BinExpr(KotlinTypes.BOOLEAN, "!=", valRef, Exprs.nullLit)
              else Exprs.is(valRef, SimpleType(ref))

            val body = BlockExpr(NoType, Seq(
              IfExpr(
                NoType,
                condition,
                BlockExpr(innerBodyExprs.last.ty, innerBodyExprs),
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
            case MatchCaseClause(LitPatternMatch(lit), e, guard) =>
              val equlasExpr = BinExpr(KotlinTypes.BOOLEAN, "==", valRef, lit)
              ExprWhenClause(addGuardExpr(equlasExpr, guard), pass[Expr](e))

            case MatchCaseClause(WildcardPatternMatch, e, guard) =>
              guard match {
                case Some(g) => ExprWhenClause(pass[Expr](g), pass[Expr](e))
                case None => ElseWhenClause(pass[Expr](e))
              }

            case MatchCaseClause(ReferencePatternMatch(ref), e, guard) =>
              scoped(
                renamesVal.updated(_.add(ref -> valRef.ref))
              ) {
                guard match {
                  case Some(g) => ExprWhenClause(pass[Expr](g), pass[Expr](e))
                  case None => ElseWhenClause(pass[Expr](e))
                }
              }

            case MatchCaseClause(TypedPatternMatch(ref, patternTy), e, guard) =>
              scoped(
                renamesVal.updated(_.add(ref -> valExpr.destructors.head.name))
              ) {
                ExprWhenClause(addGuardExpr(Exprs.is(valRef, patternTy), guard.map(pass[Expr])), pass[Expr](e))
              }

            case MatchCaseClause(pattern@ConstructorPatternMatch(ref, args, _, repr), e, _) =>
              val lazyRef = RefExpr(NoType, None, Utils.escapeName(repr), Seq.empty, false)
              val notEqulasExpr = BinExpr(KotlinTypes.BOOLEAN, "!=", lazyRef, Exprs.nullLit)
              val vals = collectVals(pattern)
              val valDef = ValDef(vals.map(p => ReferencePatternMatch(p.name)), lazyRef)
              val body = e match {
                case BlockExpr(ty, exprs) =>
                  BlockExpr(ty, valDef +: exprs)
                case expr =>
                  BlockExpr(expr.ty, Seq(valDef, expr))
              }
              ExprWhenClause(notEqulasExpr, body)
          }
          .span(_.isInstanceOf[ExprWhenClause]) match { //take all before first else and first else
            case (h, t) => h ++ t.headOption.toSeq
          }

        val whenExpr = WhenExpr(NoType, None, whenClauses)
        Some(pass[Expr](BlockExpr(whenExpr.ty, valExpr +: (caseClasses ++ lazyDefs) :+ whenExpr)))

      case _ => None
    }
  }


  private def handleAttrs(x: Defn) = {
    def attr(p: Boolean, a: Attr) =
      if (p) Some(a) else None

    val attrs =
      (attr(x.attrs.contains(CaseAttr) && x.t == ClassDefn, DataAttr) ::
        attr(!x.attrs.contains(FinalAttr) && x.t == ClassDefn && !x.attrs.contains(CaseAttr), OpenAttr) ::
        attr(x.attrs.contains(PublAttr), PublAttr) ::
        attr(x.attrs.contains(PrivAttr), PrivAttr) ::
        attr(x.attrs.contains(ProtAttr), ProtAttr) ::
        Nil)
        .flatten
    sortAttrs(attrs)
  }

  private def sortAttrs(attrs: Seq[Attr]) =
    attrs.sortBy {
      case PublAttr => 1
      case PrivAttr => 1
      case ProtAttr => 1
      case OpenAttr => 2
      case FinalAttr => 2
      case CaseAttr => 3
      case DataAttr => 3
      case OverrideAttr => 4
      case _ => 5
    }
}

