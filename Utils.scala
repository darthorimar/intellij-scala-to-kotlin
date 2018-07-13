package org.jetbrains.plugins.kotlinConverter

object Utils {
  def escapeName(name: String): String =
    s"`$name`"
}
