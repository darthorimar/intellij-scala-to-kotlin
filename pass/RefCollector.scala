package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.ast._

class RefCollector extends Transform {
  override protected def action(ast: AST): Option[AST] =
    ast match {
      case x: FileDef =>
        Some(copy(x).asInstanceOf[FileDef].copy(imports = x.imports ++ imports.toSeq))

      case ClassType(name) =>
        val importPath = name.stripPrefix(context.packageName).stripPrefix(".").stripSuffix("$")
        if (!name.startsWith(context.packageName) && name.contains("."))
          imports = imports + ImportDef(importPath)
        val className = name.split('.').last
        Some(ClassType(className))

      case _ => None
    }
}
