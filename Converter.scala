package org.jetbrains.plugins.kotlinConverter

import com.intellij.openapi.application.ApplicationManager
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.pass.Pass
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.transformation.Transformer
import org.jetbrains.plugins.scala.lang.transformation.annotations.{AddTypeToValueDefinition, AddTypeToVariableDefinition}
import org.jetbrains.plugins.scala.lang.transformation.calls._

object Converter {
  val transformers: Set[Transformer] = Set.apply(
    new ExpandApplyCall(),
    new ExpandUpdateCall(),
    new AddTypeToVariableDefinition(),
    new AddTypeToValueDefinition()
  )

  def convert(file: ScalaFile): String = {
    ApplicationManager.getApplication.runWriteAction(new Runnable {
      override def run(): Unit = Transformer.transform(file, None, transformers)
    })

    val builder: KotlinBuilder = new KotlinBuilder
    val ast: FileDef = ASTGenerator.gen[FileDef](file)
    println(Utils.prettyPrint(ast))
    val newAst: AST = Pass.applyPasses(ast)
    builder.gen(newAst)
    builder.text
  }
}
