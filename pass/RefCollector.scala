package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.ast._

class RefCollector extends Transform {
  override protected def action(ast: AST): Option[AST] =
    ast match {
      case x: FileDef =>
        Some(copy(x).asInstanceOf[FileDef].copy(imports = x.imports ++ imports.toSeq))

      case SimpleType(name) =>
        imports = imports + ImportDef(name)
        Some(SimpleType(name.split('.').last))

      case _ => None
    }
}
