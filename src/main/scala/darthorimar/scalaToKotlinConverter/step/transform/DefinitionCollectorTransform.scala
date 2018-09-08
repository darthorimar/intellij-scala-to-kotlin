package darthorimar.scalaToKotlinConverter.step.transform

import darthorimar.scalaToKotlinConverter.ast._
import darthorimar.scalaToKotlinConverter.definition.Definition
import darthorimar.scalaToKotlinConverter.types._

class DefinitionCollectorTransform extends Transform {
  override def name: String = "Collecting used definitions"

  val calls: PartialFunction[(Type, String), Definition] = {
    case (GenericType(KotlinTypes.LIST, Seq(GenericType(ClassType("Tuple3"), _))), "unzip3") =>
      Definition.unzip3
    case (GenericType(KotlinTypes.LIST | KotlinTypes.ARRAY, _), "collect") =>
      Definition.collect
    case (StdTypes.STRING, "stripSuffix") =>
      Definition.stripSuffix
  }

  override protected val action: PartialFunction[AST, AST] = {
    case c@CallExpr(_, RefExpr(_, Some(expr), name, _, _), _, _)
      if calls.isDefinedAt((expr.exprType, name)) =>
      stateStepVal.addDefinition(calls((expr.exprType, name)))
      copy[CallExpr](c)
  }
}
