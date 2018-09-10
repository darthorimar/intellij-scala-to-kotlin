package darthorimar.scalaToKotlinConverter.types

import darthorimar.scalaToKotlinConverter.ast.StdType

object StdTypes {
  val ANY = StdType("Any")
  val STRING = StdType("String")
  val ANY_REF = StdType("AnyRef")
  val NOTHING = StdType("Nothing")
  val ANY_VAL = StdType("AnyVal")
  val UNIT = StdType("Unit")
  val BOOLEAN = StdType("Boolean")
  val CHAR = StdType("Char")
  val BYTE = StdType("Byte")
  val SHORT = StdType("Short")
  val INT = StdType("Int")
  val LONG = StdType("Long")
  val FLOAT = StdType("Float")
  val DOUBLE = StdType("Double")

  val NUMERIC_TYPES = Seq(
    BYTE, SHORT, INT, LONG, FLOAT, DOUBLE
  )
}
