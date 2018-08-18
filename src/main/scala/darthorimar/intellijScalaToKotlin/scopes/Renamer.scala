package darthorimar.intellijScalaToKotlin.scopes

import darthorimar.intellijScalaToKotlin.ast.Expr

case class Renamer(renames: Map[String, Expr]) {
  def add(rename: (String, Expr)) =
    Renamer(renames + rename)

  def addAll(newRenames: Map[String, Expr]) =
    Renamer(renames ++ newRenames)
}
