package org.jetbrains.plugins.kotlinConverter.pass

import org.gradle.model.internal.manage.schema.extract.ScalarTypes
import org.jetbrains.plugins.kotlinConverter
import org.jetbrains.plugins.kotlinConverter.types._
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.pass.Pass.PasssContext

class TypePass extends Pass {
  override protected def action(ast: AST)(implicit context: PasssContext): Option[AST] = ast match {
    case PType(t, Seq(i)) if TypeUtils.isOption(t) =>
      Some(NullableType(pass[Type](i)))

    case ScalaTypes.STRING | ScalaTypes.JAVA_STRING =>
      Some(SimpleType("String"))

    case ScalaTypes.SEQ | ScalaTypes.SEQ2 =>
      Some(SimpleType("List"))

    case _ => None
  }

  override def emptyContext: PasssContext = new PasssContext {}
}

