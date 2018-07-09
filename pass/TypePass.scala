package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.ast._

class TypePass extends Pass {
  override protected def action(ast: AST): Option[AST] = ast match {
    case PType(SimpleType(tn), Seq(t))
      if tn == "_root_.scala.Option" || tn == "_root_.scala.Some" =>
      Some(NullableType(pass[Type](t)))

    case x@RefFExpr(FuncType(_, PType(Types.OPTION,_)), "map") =>
      Some(copy(x).asInstanceOf[RefFExpr].copy(name="let"))
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

