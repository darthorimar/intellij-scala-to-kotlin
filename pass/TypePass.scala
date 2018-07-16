package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.types._
import org.jetbrains.plugins.kotlinConverter.ast._

class TypePass extends Pass {
  override protected def action(ast: AST): Option[AST] = ast match {
    case ProductType(t, Seq(i)) if TypeUtils.isOption(t) =>
      Some(NullableType(pass[Type](i)))

    case ScalaTypes.STRING | ScalaTypes.JAVA_STRING =>
      Some(KotlinTypes.STRING)

    case ScalaTypes.SEQ | ScalaTypes.SEQ2 | ScalaTypes.LIST | ScalaTypes.LIST2=>
      Some(KotlinTypes.LIST)

    case _ => None
  }
}

