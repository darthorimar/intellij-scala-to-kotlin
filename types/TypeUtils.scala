package org.jetbrains.plugins.kotlinConverter.types

import org.jetbrains.plugins.kotlinConverter.ast._

object TypeUtils {
  def isOption(ty: Type): Boolean =
    ty match {
      case PType(ScalaTypes.OPTION | ScalaTypes.SOME| ScalaTypes.NONE, _) =>
        true
      case _ => false
    }

}
