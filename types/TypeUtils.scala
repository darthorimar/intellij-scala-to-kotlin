package org.jetbrains.plugins.kotlinConverter.types

import org.jetbrains.plugins.kotlinConverter.ast._

object TypeUtils {
  def isOption(ty: Type): Boolean =
    ty match {
      case GenerecTypes(t, _) if isOption(t)=>
        true
      case ScalaTypes.OPTION | ScalaTypes.SOME | ScalaTypes.NONE => true
      case _ => false
    }

  def isKotlinList(ty: Type): Boolean = ty match {
    case GenerecTypes(KotlinTypes.LIST, _) =>
      true
    case KotlinTypes.LIST => true
    case _ => false
  }

}
