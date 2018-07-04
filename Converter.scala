package org.jetbrains.plugins.kotlinConverter

import org.jetbrains.plugins.kotlinConverter.ast.Stmt.FileDef
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile

object Converter {
  def convert(file: ScalaFile): String = {
    val builder = new KotlinBuilder
    builder.gen(ASTConverter.gen[FileDef](file))
    builder.text
  }
}
