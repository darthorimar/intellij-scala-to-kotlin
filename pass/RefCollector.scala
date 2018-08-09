package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.ast._

class RefCollector extends Transform {
  override protected def action(ast: AST): Option[AST] =
    ast match {
//      case f: File if f.neededDefinitions.nonEmpty =>
//        imports = imports + ""
      case ClassType(name) =>
        val importPath = name.stripPrefix(context.packageName).stripPrefix(".").stripSuffix("$")
        if (!name.startsWith(context.packageName) && name.contains("."))
          imports = imports + Import(importPath)
        val className = name.split('.').last
        Some(ClassType(className))

      case _ => None
    }
}
