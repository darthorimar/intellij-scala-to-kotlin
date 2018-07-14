package org.jetbrains.plugins.kotlinConverter.pass

case class Renames(renames: Map[String, String]) {
  def add(rename: (String, String)) =
    Renames(renames + rename)
}