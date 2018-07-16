package org.jetbrains.plugins.kotlinConverter

import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.pass.Pass
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile

object Converter {
  def convert(file: ScalaFile): String = {
    val builder = new KotlinBuilder
    val ast = ASTGenerator.gen[FileDef](file)
    println(Utils.prettyPrint(ast))
    val newAst = Pass.applyPasses(ast)
    builder.gen(newAst)
    builder.text
  }
}
