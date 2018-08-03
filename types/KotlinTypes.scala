package org.jetbrains.plugins.kotlinConverter.types

import org.jetbrains.plugins.kotlinConverter.ast.{SimpleType, Type}

object KotlinTypes {
  val UNIT = SimpleType("Unit")
  val PAIR = SimpleType("Pair")
  val BOOLEAN = SimpleType("Boolean")
  val NOTHING = SimpleType("Nothing")
  val STRING = SimpleType("String")
  val LIST = SimpleType("List")
  val INT = SimpleType("Int")
  val THROWABLE = SimpleType("Throwable")
}
