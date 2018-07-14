package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.types._
import org.jetbrains.plugins.kotlinConverter.ast._

class TypePass extends Pass {
  override protected def action(ast: AST): Option[AST] = ast match {
    case PType(t, Seq(i)) if TypeUtils.isOption(t) =>
      Some(NullableType(pass[Type](i)))

    case ScalaTypes.STRING | ScalaTypes.JAVA_STRING =>
      Some(SimpleType("String"))

    case ScalaTypes.SEQ | ScalaTypes.SEQ2 =>
      Some(SimpleType("List"))

    case _ => None
  }
}

