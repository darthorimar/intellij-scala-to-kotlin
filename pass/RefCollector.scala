package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.ast._

class RefCollector extends Transform {
  override protected def action(ast: AST): Option[AST] =
    ast match {
      case ClassType(name) =>
        val className = addImport(name)
        Some(ClassType(className))
      case JavaType(name) =>
        val className = addImport(name)
        Some(JavaType(className))
      case r@RefExpr(_, None, name, _, _) =>
        val className = addImport(name)
        Some(copy(r).asInstanceOf[RefExpr].copy(referenceName = className))
      case _ => None
    }

  private def addImport(name: String) = {
    if (name.startsWith("`") && name.endsWith("`")) name
    else {
      val importPath = name.stripSuffix("$")
      if (name.contains(".") && !name.startsWith("scala."))
        imports = imports + Import(importPath)
      val className = name.split('.').last
      className
    }
  }
}
