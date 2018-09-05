package darthorimar.scalaToKotlinConverter.step.transform

import darthorimar.scalaToKotlinConverter.ast._

class CollectImportsStep extends Transform {
  override protected val action: PartialFunction[AST, AST] = {
      case ClassType(name) =>
        val className = addNewImport(name)
        ClassType(className)
      case JavaType(name) =>
        val className = addNewImport(name)
        JavaType(className)
      case r@RefExpr(_, None, name, _, _) =>
        val className = addNewImport(name)
        copy(r).asInstanceOf[RefExpr].copy(referenceName = className)
    }

  private def addNewImport(name: String) = {
    if (name.startsWith("`") && name.endsWith("`")) name
    else {
      val importPath = name.stripSuffix("$")
      if (name.contains(".") && !name.startsWith("scala."))
        stateStepVal.addImport(Import(importPath))
      val className = name.split('.').last
      className
    }
  }
}