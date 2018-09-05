package darthorimar.scalaToKotlinConverter.step.transform

import darthorimar.scalaToKotlinConverter.ast._
import darthorimar.scalaToKotlinConverter.definition.Definition
import darthorimar.scalaToKotlinConverter.types._

class DefinitionCollectorTransform extends Transform {

  override protected def action(ast: AST): Option[AST] = ast match {
    case CallExpr(_, RefExpr(_, Some(expr), name, _, _), _, _)
      if calls.isDefinedAt((expr.exprType, name)) =>
      stateStepVal.addDefinition(calls((expr.exprType, name)))
      None
    case _ => None
  }

  val calls: PartialFunction[(Type, String), Definition] = {
    case (GenericType(KotlinTypes.LIST, Seq(GenericType(ClassType("Tuple3"), _))), "unzip3") =>
      Definition.unzip3
    case (GenericType(KotlinTypes.LIST | KotlinTypes.ARRAY, _), "collect") =>
      Definition.collect
    case (StdTypes.STRING, "stripSuffix") =>
      Definition.stripSuffix
  }
}
