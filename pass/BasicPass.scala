package org.jetbrains.plugins.kotlinConverter.pass

import com.sun.source.doctree.AttributeTree.ValueKind
import org.jetbrains.plugins.kotlinConverter
import org.jetbrains.plugins.kotlinConverter.{Exprs, Utils}
import org.jetbrains.plugins.kotlinConverter.types.KotlinTypes
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.pass.ScopedVal.scoped


class BasicPass extends Pass {
  val renamesVal = new ScopedVal[Renames](Renames(Map.empty))
  val namerVal = new ScopedVal[LocalNamer](new LocalNamer)

  override protected def action(ast: AST): Option[AST] = {
    ast match {
      case x: RefExpr if renamesVal.call(_.renames.contains(x.ref)) =>
        Some(x.copy(ref = renamesVal.get.renames(x.ref)))

      //Remove renault brackets for lambda like in seq.map {x => x * 2}
      case BlockExpr(stmts) if stmts.size == 1 && stmts.head.isInstanceOf[LambdaExpr] =>
        Some(pass[Expr](stmts.head))

      case ParamsConstruct(params)
        if parent.asInstanceOf[Defn].attrs.contains(CaseAttr) =>
        Some(ParamsConstruct(params.map {
          case ConstructParam(parType, mod, name, ty) =>
            val t = if (parType == NoMemberKind) ValKind else parType
            val m = if (mod == NoAttr) PublAttr else mod
            ConstructParam(t, m, name, pass[Type](ty))
        }))

      case x: DefnDef =>
        scoped(
          namerVal.set(new LocalNamer)
        ) {
          Some(copy(x).asInstanceOf[DefnDef].copy(attrs = sortAttrs(x.attrs)))
        }

      case x: Defn =>
        val defn = copy(x).asInstanceOf[Defn]
        val t =
          if (x.t == TraitDefn) InterfaceDefn
          else x.t
        Some(copy(defn).asInstanceOf[Defn].copy(attrs = handleAttrs(defn), t = t))

      //uncarry
      case x@CallExpr(_, _: CallExpr, _) =>
        def collectParams(c: Expr): List[Expr] = c match {
          case x: CallExpr =>
            collectParams(x.ref) ++ x.params.toList
          case _ => Nil
        }

        def collectRef(c: CallExpr): Expr = c.ref match {
          case x: CallExpr => collectRef(x)
          case _ => c.ref
        }

        val params = collectParams(x)
        val ref = collectRef(x)
        Some(CallExpr(
          pass[Type](x.ty),
          pass[Expr](ref),
          params.map(pass[Expr])))

      case x@RefExpr(ty, obj, ref, typeParams, true)
        if (parent match {
          case CallExpr(_, r, _) if r == x => false
          case _ => true
        }) =>
        Some(CallExpr(ty, copy(x).asInstanceOf[RefExpr], Seq.empty))

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
                  CallExpr(pass[Type](y.ty), pass[Expr](y), Seq(UnderScExpr(y.ty))),
                  false)
            }))

      // val destructing in a case of nested constructors
      //      case ValDef(destructors, expr)
      //        if destructors.exists {
      //          case _: ConstructorPatternMatch => true
      //          case _ => false
      //        } =>


      // match expr to when one
      case MatchExpr(ty, expr, clauses) =>
        val badClauses = clauses.collect {
          case x@MatchCaseClause(_: ConstructorPatternMatch, _, _) => x
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

        val caseClasses = badClauses.map { case MatchCaseClause(pattern@ConstructorPatternMatch(_, _, _, repr), _, _) =>
          val name = Utils.escapeName(s"${repr}_data")
          val vals = collectVals(pattern)
          Defn(Seq(DataAttr),
            ClassDefn,
            name,
            Some(ParamsConstruct(vals)),
            Seq.empty,
            None)
        }

        def collectConstructors(constructors: Seq[(String, ConstructorPatternMatch)]): (Seq[ValDef], Seq[Expr], Seq[(String, ConstructorPatternMatch)]) = {
          val (vals, conds, refs) = constructors.map { case (r, ConstructorPatternMatch(_, patterns, _, _)) =>
            val (destructors, conds, refs) = patterns.map {
              case LitPatternMatch(litPattern) =>
                (LitPatternMatch(litPattern), None, None)
              case ReferencePatternMatch(ref) =>
                (ReferencePatternMatch(ref), None, None)
              case WildcardPatternMatch =>
                (WildcardPatternMatch, None, None)
              case c@ConstructorPatternMatch(ref, _, label, _) =>
                val local = label.getOrElse(namerVal.get.newName("l")) //todo use name from pattern
                (ReferencePatternMatch(local),
                  Some(Exprs.is(LitExpr(ty, local), SimpleType(ref))),
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

        def handleConstructors(constructors: Seq[(String, ConstructorPatternMatch)], defaultCase: Expr): Expr = {
          val (valDefns, conditionParts, collectedConstructors) = collectConstructors(constructors)

          val trueBlock =
            if (collectedConstructors.nonEmpty) {
              handleConstructors(collectedConstructors, defaultCase)
            } else defaultCase

          val ifCond =
            if (conditionParts.nonEmpty)
              IfExpr(NoType, conditionParts.reduceLeft(Exprs.and), trueBlock, None)
            else trueBlock

          BlockExpr(valDefns :+ ifCond)
        }

        val lazyDefs = badClauses.map {
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

            val innerBody = handleConstructors(Seq((valRef.ref, pattern)), finalExpr)
            val body = BlockExpr(Seq(
              IfExpr(
                NoType,
                Exprs.is(valRef, SimpleType(ref)),
                innerBody,
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
          clauses.map {
            case MatchCaseClause(LitPatternMatch(lit), e, guard) =>
              val equlasExpr = BinExpr(KotlinTypes.BOOLEAN, BinOp("=="), valRef, lit)
              ExprWhenClause(addGuardExpr(equlasExpr, guard), pass[Expr](e))

            case MatchCaseClause(WildcardPatternMatch, e, guard) =>
              guard match {
                case Some(g) => ExprWhenClause(pass[Expr](g), pass[Expr](e))
                case None => ElseWhenClause(pass[Expr](e))
              }

            case MatchCaseClause(ReferencePatternMatch(ref), e, guard) =>
              ScopedVal.scoped(
                renamesVal.updated(_.add(ref -> valRef.ref))) {
                guard match {
                  case Some(g) => ExprWhenClause(pass[Expr](g), pass[Expr](e))
                  case None => ExprWhenClause(Exprs.trueLit, pass[Expr](e))
                }
              }

            case MatchCaseClause(TypedPatternMatch(ref, patternTy), e, guard) =>
              ScopedVal.scoped(
                renamesVal.updated(_.add(ref -> valExpr.destructors.head.name))) {
                ExprWhenClause(addGuardExpr(Exprs.is(valRef, patternTy), guard.map(pass[Expr])), pass[Expr](e))
              }

            case MatchCaseClause(pattern@ConstructorPatternMatch(ref, args, _, repr), e, _) =>
              val lazyRef = RefExpr(NoType, None, Utils.escapeName(repr), Seq.empty, false)
              val notEqulasExpr = BinExpr(KotlinTypes.BOOLEAN, BinOp("!="), lazyRef, Exprs.nullLit)
              val vals = collectVals(pattern)
              val valDef = ValDef(vals.map(p => ReferencePatternMatch(p.name)), lazyRef)
              val body = BlockExpr(Seq(valDef, e))
              ExprWhenClause(notEqulasExpr, body)

          }
        val whenExpr = WhenExpr(NoType, None, whenClauses)
        Some(BlockExpr(valExpr +: (caseClasses ++ lazyDefs) :+ whenExpr))

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

