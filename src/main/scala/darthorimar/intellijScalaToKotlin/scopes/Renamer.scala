package org.jetbrains.plugins.kotlinConverter.scopes

import org.jetbrains.plugins.kotlinConverter.ast.Expr

case class Renamer(renames: Map[String, Expr]) {
  def add(rename: (String, Expr)) =
    Renamer(renames + rename)

  def addAll(newRenames: Map[String, Expr]) =
    Renamer(renames ++ newRenames)
}
