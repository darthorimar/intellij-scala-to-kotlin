package darthorimar.scalaToKotlinConverter.step.transform

import darthorimar.scalaToKotlinConverter.ast._

class CollectImportsStep extends Transform {
  override protected def action(ast: AST): Option[AST] =
    ast match {
      case ClassType(name) =>
        val className = addNewImport(name)
        Some(ClassType(className))
      case JavaType(name) =>
        val className = addNewImport(name)
        Some(JavaType(className))
      case r@RefExpr(_, None, name, _, _) =>
        val className = addNewImport(name)
        Some(copy(r).asInstanceOf[RefExpr].copy(referenceName = className))
      case _ => None
    }

  private def addNewImport(name: String) = {
    if (name.startsWith("`") && name.endsWith("`")) name
    else {
      val importPath = name.stripSuffix("$")
      if (name.contains(".") && !name.startsWith("scala."))
        stateStepVal.addImport(Import(importPath, importAll = false))
      val className = name.split('.').last
      className
    }
  }
}