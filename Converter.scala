package org.jetbrains.plugins.kotlinConverter

import com.intellij.openapi.application.ApplicationManager
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.pass.Pass
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.transformation.Transformer
import org.jetbrains.plugins.scala.lang.transformation.calls._

object Converter {
  val transformers: Set[Transformer] = Set(
    new ExpandApplyCall(),
    new ExpandUpdateCall(),
    new CanonizeZeroArityCall()
  )

  def convert(file: ScalaFile): String = {
    ApplicationManager.getApplication.runWriteAction(new Runnable {
      override def run(): Unit = Transformer.transform(file, None, transformers)
    })

    val builder = new KotlinBuilder
    val ast = ASTGenerator.gen[FileDef](file)
    println(Utils.prettyPrint(ast))
    val newAst = Pass.applyPasses(ast)
    builder.gen(newAst)
    builder.text
  }
}
