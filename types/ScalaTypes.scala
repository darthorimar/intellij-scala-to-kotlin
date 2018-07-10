package org.jetbrains.plugins.kotlinConverter.types

import org.jetbrains.plugins.kotlinConverter.ast.SimpleType

object ScalaTypes {
  val OPTION = SimpleType("_root_.scala.Option")
  val SOME = SimpleType("_root_.scala.Some")
  val NONE = SimpleType("_root_.scala.None")
  val STRING = SimpleType("_root_.scala.Predef.String")
  val SEQ = SimpleType("scala.Seq")

  val FUNCTION_PREFFIX = "_root_.scala.Function"
}
