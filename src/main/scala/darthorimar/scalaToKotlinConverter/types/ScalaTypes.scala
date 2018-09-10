package darthorimar.scalaToKotlinConverter.types

import darthorimar.scalaToKotlinConverter.ast._

object ScalaTypes {
  val OPTION                    = ScalaType("scala.Option")
  val SOME                      = ScalaType("scala.Some")
  val NONE                      = ScalaType("scala.None$")
  val STRING                    = SimpleType("scala.Predef.String")
  val JAVA_STRING               = JavaType("java.lang.String")
  val COLLECTION_SEQ            = ScalaType("scala.collection.Seq")
  val SEQ                       = ScalaType("scala.Seq")
  val COLLECTION_LIST           = ScalaType("scala.collection.List")
  val COLLECTION_IMMUTABLE_LIST = ScalaType("scala.collection.immutable.List")
  val LIST                      = ScalaType("scala.List")

  val FUNCTION_PREFFIX = "_root_.scala.Function"

}
