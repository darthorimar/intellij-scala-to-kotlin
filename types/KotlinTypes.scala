package org.jetbrains.plugins.kotlinConverter.types

import org.jetbrains.plugins.kotlinConverter.ast.{KotlinCollectionType, SimpleType, Type}

object KotlinTypes {
  val UNIT = SimpleType("Unit")
  val BOOLEAN = SimpleType("Boolean")
  val NOTHING = SimpleType("Nothing")
  val STRING = SimpleType("String")
  val INT = SimpleType("Int")
  val THROWABLE = SimpleType("Throwable")
  val LIST = KotlinCollectionType("List")
  val PAIR = KotlinCollectionType("Pair")
}
