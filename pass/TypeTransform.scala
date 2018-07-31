package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.types._
import org.jetbrains.plugins.kotlinConverter.ast._

class TypeTransform extends Transform {
  override protected def action(ast: AST): Option[AST] = ast match {
    case GenerecTypes(inner, Seq(i)) if TypeUtils.isOption(inner) =>
      Some(NullableType(pass[Type](i)))

    case ScalaTypes.STRING | ScalaTypes.JAVA_STRING =>
      Some(KotlinTypes.STRING)

    case ScalaTypes.SEQ | ScalaTypes.SEQ2 | ScalaTypes.LIST | ScalaTypes.LIST2 =>
      Some(KotlinTypes.LIST)


    case SimpleType(name) if name.startsWith("_root_.") =>
      Some(SimpleType(name.stripPrefix("_root_.")))

    case _ => None
  }
}

