package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.ast._

class TypePass extends Pass {
  override protected def action(ast: AST): Option[AST] = ast match {
    case PType(SimpleType(tn), Seq(t))
      if tn == "_root_.scala.Option" || tn == "_root_.scala.Some" =>
      Some(NullableType(pass[Type](t)))

    case x@RefExpr(FuncType(_, PType(Types.OPTION,_)), "map") =>
      Some(copy(x).asInstanceOf[RefExpr].copy(name="let"))

    case Types.STRING =>
      Some(SimpleType("String"))

    case Types.SEQ =>
      Some(SimpleType("List"))

    case _ => None
  }

}

