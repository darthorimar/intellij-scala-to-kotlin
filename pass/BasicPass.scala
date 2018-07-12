package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter
import org.jetbrains.plugins.kotlinConverter.types.KotlinTypes
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.pass.Pass.PasssContext

import scala.collection.mutable

class BasicPass extends Pass {
  override protected def action(ast: AST)(implicit context: PasssContext): Option[AST] = {
    ast match {
      case x: RefExpr if context.asInstanceOf[Context].renames.contains(x.ref) =>
        Some(x.copy(ref = context.asInstanceOf[Context].renames(x.ref)))

      //Remove renault brackets for lambda like in seq.map {x => x * 2}
      case MultiBlock(stmts) if stmts.size == 1 && stmts.head.isInstanceOf[LambdaExpr] =>
        Some(SingleBlock(pass[Expr](stmts.head)))

      case ParamsConstruct(params)
        if parent.asInstanceOf[Defn].attrs.contains(CaseAttr) =>
        Some(ParamsConstruct(params.map {
          case ConstructParam(parType, mod, name, ty) =>
            val t = if (parType == NoMemberKind) VarlKind else parType
            val m = if (mod == NoAttr) PublAttr else mod
            ConstructParam(t, m, name, pass[Type](ty))
        }))


      case x: Defn =>
        val defn = copy(x).asInstanceOf[Defn]
        val t =
          if (x.t == TraitDefn) InterfaceDefn
          else x.t
        Some(defn.copy(attrs = handleAttrs(defn), t = t))

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

      //a.call --> a.call()
      case x@CallExpr(ty, ref, params)
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
                  CallExpr(pass[Type](y.ty), pass[Expr](y), Seq(UnderScExpr(y.ty))))
            }))

      // match -> when when possible
      case MatchExpr(ty, expr, clauses)
        if clauses.map(_.pattern).forall {
          case _: LitPatternMatch => true
          case WildcardPatternMatch => true
          case _: ReferencePatternMatch => true
          case _: TypedPatternMatch => true
          case _ => false
        } && clauses.map(_.guard).forall(_.isEmpty) =>
        val newExpr = pass[Expr](expr)
        val valExpr = ValDef("match$", newExpr.ty, newExpr)
        val whenClauses = clauses.map {
          case MatchCaseClause(LitPatternMatch(lit), e, _) =>
            ExprWhenClause(pass[Expr](lit), pass[Expr](e))
          case MatchCaseClause(WildcardPatternMatch, e, _) =>
            ElseWhenClause(pass[Expr](e))
          case MatchCaseClause(ReferencePatternMatch(ref), e, _) =>
            implicit val newContext: Context =
              context.asInstanceOf[Context]
                .copy(renames = context.asInstanceOf[Context].renames + (ref -> valExpr.name))
            ExprWhenClause(LitExpr(SimpleType("Boolean"), "true"), pass[Expr](e))

          case MatchCaseClause(TypedPatternMatch(ref, patternTy), e, _) =>
            implicit val newContext: Context =
              context.asInstanceOf[Context]
                .copy(renames = context.asInstanceOf[Context].renames + (ref -> valExpr.name))
            ExprWhenClause(BinExpr(KotlinTypes.BOOLEAN, BinOp("is"), LitExpr(NoType, ""), TypeExpr(patternTy)), pass[Expr](e))
        }
        val whenExpr = kotlinConverter.ast.WhenExpr(ty, Some(LitExpr(newExpr.ty, valExpr.name)), whenClauses)
        Some(MultiBlock(Seq(valExpr, whenExpr)))

      case _ => None
    }
  }

  private def handleAttrs(x: Defn) = {
    def attr(p: Boolean, a: Attr) =
      if (p) Some(a) else None

    def comparator(attr: Attr) = attr match {
      case PublAttr => 1
      case PrivAttr => 1
      case ProtAttr => 1
      case OpenAttr => 2
      case FinalAttr => 2
      case CaseAttr => 3
      case DataAttr => 3
      case _ => 4
    }

    (attr(x.attrs.contains(CaseAttr) && x.t == ClassDefn, DataAttr) ::
      attr(!x.attrs.contains(FinalAttr) && x.t == ClassDefn && !x.attrs.contains(CaseAttr), OpenAttr) ::
      attr(x.attrs.contains(PublAttr), PublAttr) ::
      attr(x.attrs.contains(PrivAttr), PrivAttr) ::
      attr(x.attrs.contains(ProtAttr), ProtAttr) ::
      Nil)
      .flatten
      .sortBy(comparator)
  }

  override def emptyContext: PasssContext = Context(Map.empty)
}

case class Context(renames: Map[String, String]) extends PasssContext