package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.ast._

class BasicPass extends Pass {
  override protected def action(ast: AST): Option[AST] = ast match {
    //Remove renault brackets for lambda like in seq.map {x => x * 2}
    case MultiBlock(stmts) if stmts.size == 1 && stmts.head.isInstanceOf[LambdaExpr] =>
      Some(SingleBlock(pass[Expr](stmts.head)))

    case ParamsConstruct(params)
      if parent.asInstanceOf[Defn].attrs.contains(CaseAttr) =>
      Some(ParamsConstruct(params.map {
        case ConstructParam(parType, mod, name, ty) =>
          val t = if (parType == NoParamType) ValType else parType
          val m = if (mod == NoModifier) PublModifier else mod
          ConstructParam(t, m, name, pass[Type](ty))
      }))


    case x: Defn =>
      val defn = copy(x).asInstanceOf[Defn]
      val t =
        if (x.t == TraitDefn) InterfaceDefn
        else x.t
      Some(defn.copy(attrs = handleAttrs(defn.attrs.toList), t = t))

    //uncarry
    case x@CallExpr(_, _: CallExpr, _, _) =>
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
        x.typeParams.map(pass[TypeParam]),
        params.map(pass[Expr])))

    case PType(SimpleType(tn), Seq(t))
      if tn == "_root_.scala.Option" || tn == "_root_.scala.Some" =>
      Some(NulableType(pass[Type](t)))


    case _ => None
  }

  private def handleAttrs(attrs: List[Attr]) = {
    def comparator(attr: Attr) = attr match {
      case PublAttr => 1
      case PrivAttr => 1
      case ProtAttr => 1
      case OpenAttr => 2
      case FinalAttr => 2
      case CaseAttr => 3
    }

    (if (attrs.contains(FinalAttr)) attrs.filter(_ == FinalAttr)
    else if (!attrs.contains(CaseAttr)) OpenAttr :: attrs
    else attrs)
      .sortBy(comparator)
  }
}

