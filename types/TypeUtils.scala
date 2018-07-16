package org.jetbrains.plugins.kotlinConverter.types

import org.jetbrains.plugins.kotlinConverter.ast._

object TypeUtils {
  def isOption(ty: Type): Boolean =
    ty match {
      case ProductType(ScalaTypes.OPTION | ScalaTypes.SOME | ScalaTypes.NONE, _) =>
        true
      case ScalaTypes.OPTION | ScalaTypes.SOME | ScalaTypes.NONE => true
      case _ => false
    }

}
