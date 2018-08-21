package org.jetbrains.plugins.kotlinConverter.types

import org.jetbrains.plugins.kotlinConverter.ast._

object ScalaTypes {
  val OPTION = ScalaType("scala.Option")
  val SOME = ScalaType("scala.Some")
  val NONE = ScalaType("scala.None$")
  val STRING = SimpleType("scala.Predef.String")
  val JAVA_STRING = JavaType("java.lang.String")
  val SEQ = ScalaType("scala.collection.Seq")
  val SEQ2 = ScalaType("scala.Seq")
  val LIST = ScalaType("scala.collection.List")
  val LIST2 = ScalaType("scala.collection.immutable.List")
  val LIST3 = ScalaType("scala.collection.immutable.List")
  val LIST4 = ScalaType("scala.List")

  val FUNCTION_PREFFIX = "_root_.scala.Function"

}
