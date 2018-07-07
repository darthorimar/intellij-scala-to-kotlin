package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.ast.Expr._
import org.jetbrains.plugins.kotlinConverter.ast.Stmt._

class BasicPass extends Pass {
  override protected def action(ast: AST): Option[AST] = ast match {
    //Remove renault brackets for lambda like in seq.map {x => x * 2}
    case MultiBlock(stmts) if stmts.size == 1 && stmts.head.isInstanceOf[Lambda] =>
      Some(SingleBlock(pass[Expr](stmts.head)))

    case ParamsConstruct(params)
      if parent.asInstanceOf[Defn].attrs.contains(CaseAttr) =>
      Some(ParamsConstruct(params.map {
        case ConstructParam(parType, mod, name, ty) =>
          val t = if (parType == NoType) ValType else parType
          val m = if (mod == NoModifier) PublModifier else mod
          ConstructParam(t, m, name, pass[TypeCont](ty))
      }))

    //handle def attrs
    case x: Defn =>
      val defn = copy(x).asInstanceOf[Defn]
      Some(defn.copy(attrs = handleAttrs(defn.attrs.toList)))

    //uncarry
    case x@Call(_, _: Call, _, _) =>
      def collectParams(c: Expr): List[Expr] = c match {
        case x: Call =>
          collectParams(x.ref) ++ x.params.toList
        case _ => Nil
      }

      def collectRef(c: Call): Expr = c.ref match {
        case x: Call => collectRef(x)
        case _ => c.ref
      }

      val params = collectParams(x)
      val ref = collectRef(x)
      Some(Call(
        pass[TypeCont](x.ty),
        pass[Expr](ref),
        x.typeParams.map(pass[TypeParam]),
        params.map(pass[Expr])))

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

