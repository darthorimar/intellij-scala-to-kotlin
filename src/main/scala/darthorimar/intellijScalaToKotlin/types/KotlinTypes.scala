package darthorimar.intellijScalaToKotlin.types

import darthorimar.intellijScalaToKotlin.ast.{KotlinType, StdType, SimpleType, Type}

object KotlinTypes {
  val THROWABLE = KotlinType("Throwable")
  val EXCEPTION = KotlinType("Exception")
  val LIST = KotlinType("List")
  val PAIR = KotlinType("Pair")
  val ARRAY = KotlinType("Array")
}
