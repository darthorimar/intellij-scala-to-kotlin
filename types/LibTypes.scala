package org.jetbrains.plugins.kotlinConverter.types

import org.jetbrains.plugins.kotlinConverter.ast._

object LibTypes {
  def tupleType(arity: Int) = ClassType(s"Tuple$arity")
}
