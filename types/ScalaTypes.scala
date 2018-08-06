package org.jetbrains.plugins.kotlinConverter.types

import org.jetbrains.plugins.kotlinConverter.ast._

object ScalaTypes {
  val OPTION = ScalaCollectionType("scala.Option")
  val SOME = ScalaCollectionType("scala.Some")
  val NONE = ScalaCollectionType("scala.None$")
  val STRING = SimpleType("scala.Predef.String")
  val JAVA_STRING = JavaType("java.lang.String")
  val SEQ = ScalaCollectionType("scala.collection.Seq")
  val SEQ2 = ScalaCollectionType("scala.Seq")
  val LIST = ScalaCollectionType("scala.collection.List")
  val LIST2 = ScalaCollectionType("scala.collection.immutable.List")
  val LIST3 = ScalaCollectionType("scala.collection.immutable.List")
  val LIST4 = ScalaCollectionType("scala.List")

  val FUNCTION_PREFFIX = "_root_.scala.Function"
}
