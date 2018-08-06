package org.jetbrains.plugins.kotlinConverter.types

import org.jetbrains.plugins.kotlinConverter.ast.{KotlinCollectionType, ScalaStdType, SimpleType, Type}

object KotlinTypes {
  val UNIT = ScalaStdType("Unit")
  val BOOLEAN = ScalaStdType("Boolean")
  val NOTHING = ScalaStdType("Nothing")
  val STRING = ScalaStdType("String")
  val INT = ScalaStdType("Int")
  val THROWABLE = KotlinCollectionType("Throwable")
  val LIST = KotlinCollectionType("List")
  val PAIR = KotlinCollectionType("Pair")
}
