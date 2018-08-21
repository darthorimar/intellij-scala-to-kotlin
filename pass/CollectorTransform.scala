package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.definition.Definition
import org.jetbrains.plugins.kotlinConverter.types._

class CollectorTransform extends Transform {

  override protected def action(ast: AST): Option[AST] = ast match {
    case CallExpr(_, RefExpr(_, Some(expr), name, _, _), _, _)
      if calls.isDefinedAt((expr.exprType, name)) =>
      addDefinition(calls((expr.exprType, name)))
      None
    case _ => None
  }

  val calls: PartialFunction[(Type, String), Definition] = {
    case (GenericType(KotlinTypes.LIST, Seq(GenericType(ClassType("Tuple3"), _))), "unzip3") =>
      Definition.unzip3
    case (GenericType(KotlinTypes.LIST, _), "collect") =>
      Definition.listCollect
    case (GenericType(KotlinTypes.ARRAY, _), "collect") =>
      Definition.arrayCollect
  }
}
