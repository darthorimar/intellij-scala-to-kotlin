package org.jetbrains.plugins.kotlinConverter.types

import org.jetbrains.plugins.kotlinConverter.ast.SimpleType

object ScalaTypes {
  val OPTION = SimpleType("_root_.scala.Option")
  val SOME = SimpleType("_root_.scala.Some")
  val NONE = SimpleType("_root_.scala.None")
  val STRING = SimpleType("_root_.scala.Predef.String")
  val JAVA_STRING = SimpleType("_root_.java.lang.String")
  val SEQ = SimpleType("_root_.scala.collection.Seq")
  val SEQ2 = SimpleType("scala.Seq")
  val LIST = SimpleType("_root_.scala.collection.List")

  val FUNCTION_PREFFIX = "_root_.scala.Function"
}
