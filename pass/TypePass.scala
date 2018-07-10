package org.jetbrains.plugins.kotlinConverter.pass

import org.gradle.model.internal.manage.schema.extract.ScalarTypes
import org.jetbrains.plugins.kotlinConverter.types._
import org.jetbrains.plugins.kotlinConverter.ast._

class TypePass extends Pass {
  override protected def action(ast: AST): Option[AST] = ast match {
    case PType(SimpleType(tn), Seq(t))
      if tn == "_root_.scala.Option" || tn == "_root_.scala.Some" =>
      Some(NullableType(pass[Type](t)))

//    case x@RefExpr(FuncType(_, PType(Types.OPTION,_)), "map") =>
//      Some(copy(x).asInstanceOf[RefExpr].copy(name="let"))
    case InvExpr(ty@FuncType(_, t), obj, "map") if TypeUtils.isOption(t) =>
      Some(InvExpr(pass[Type](ty), obj.map(x => pass[Expr](PostExpr(pass[Type](ty), x, "?"))), "let"))

    case CallExpr(_,InvExpr(ty, Some(obj), "get"), _, _) if TypeUtils.isOption(obj.ty) =>
      Some(PostExpr(pass[Type](ty), pass[Expr](obj), "!!"))

    case ScalaTypes.STRING =>
      Some(SimpleType("String"))

    case ScalaTypes.SEQ =>
      Some(SimpleType("List"))

    case _ => None
  }

}

