package org.jetbrains.plugins.kotlinConverter.types

import org.jetbrains.plugins.kotlinConverter.ast.{KotlinType, StdType, SimpleType, Type}

object KotlinTypes {
  val THROWABLE = KotlinType("Throwable")
  val EXCEPTION = KotlinType("Exception")
  val LIST = KotlinType("List")
  val PAIR = KotlinType("Pair")
}
