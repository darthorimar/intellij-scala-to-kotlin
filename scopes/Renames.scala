package org.jetbrains.plugins.kotlinConverter.scopes

case class Renames(renames: Map[String, String]) {
  def add(rename: (String, String)) =
    Renames(renames + rename)
}
