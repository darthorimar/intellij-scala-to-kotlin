package darthorimar.scalaToKotlinConverter.types

import darthorimar.scalaToKotlinConverter.ast._

object TypeUtils {

  object OptionType {
    def unapply(ty: Type): Option[Type] = ty match {
      case GenericType(ScalaTypes.OPTION | ScalaTypes.SOME, Seq(inner)) =>
        Some(inner)
      case ScalaTypes.NONE =>
        Some(StdTypes.NOTHING)
      case _ => None
    }
  }

  object KotlinList {
    def unapply(ty: Type): Option[Type] = ty match {
      case GenericType(KotlinTypes.LIST, Seq(inner)) =>
        Some(inner)
      case _ => None
    }
  }

  object NumericType {
    def unapply(t: Type): Option[Type] =
      if (StdTypes.NUMERIC_TYPES contains t) Some(t)
      else None
  }

  object WithType {
    def unapply(expr: Expr): Option[Type] = Some(expr.exprType)
  }

  object ListType {
    def unapply(t: Type): Option[Type] = t match {
      case GenericType(KotlinTypes.LIST, Seq(of)) => Some(of)
      case _                                      => None
    }
  }

  object ScalaTuple {
    private val tuplePrefix = "scala.Tuple"

    def unapply(t: Type): Option[Int] = t match {
      case ScalaType(name) if name.startsWith(tuplePrefix) =>
        val arity = name.stripPrefix(tuplePrefix).toInt
        Some(arity)
      case _ => None
    }
  }

}
