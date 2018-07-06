package org.jetbrains.plugins.kotlinConverter

import org.jetbrains.plugins.kotlinConverter.ast.Stmt.FileDef
import org.jetbrains.plugins.kotlinConverter.pass.Pass
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile

object Converter {
  def convert(file: ScalaFile): String = {
    val builder = new KotlinBuilder
    val ast = ASTGenerator.gen[FileDef](file)
    println(ast)
    val newAst = Pass.applyPasses(ast)
    println(newAst)
    builder.gen(newAst)
    builder.text
  }
}
