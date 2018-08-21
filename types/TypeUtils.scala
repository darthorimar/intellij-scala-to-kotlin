package org.jetbrains.plugins.kotlinConverter.types

import org.jetbrains.plugins.kotlinConverter.ast._

object TypeUtils {
  def isOption(ty: Type): Boolean =
    ty match {
      case GenericType(t, _) if isOption(t)=>
        true
      case ScalaTypes.OPTION | ScalaTypes.SOME | ScalaTypes.NONE => true
      case _ => false
    }

  def isKotlinList(ty: Type): Boolean = ty match {
    case GenericType(KotlinTypes.LIST, _) =>
      true
    case KotlinTypes.LIST => true
    case _ => false
  }

  object NumericType {
    def unapply(t: Type): Option[Type] = t match {
      case StdTypes.INT => Some(t)
      case _ => None
    }
  }

  object WithType {
    def unapply(expr: Expr): Option[Type] = Some(expr.exprType)
  }

  object ListType {
    def unapply(t: Type): Option[Type] = t match {
      case GenericType(KotlinTypes.LIST, Seq(of)) => Some(of)
      case _ => None
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
