package org.jetbrains.plugins.kotlinConverter

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.vfs.VirtualFile
import org.jetbrains.plugins.kotlinConverter.ast._
import org.jetbrains.plugins.kotlinConverter.builder.KotlinBuilder
import org.jetbrains.plugins.kotlinConverter.definition.Definition
import org.jetbrains.plugins.kotlinConverter.pass.Transform
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.transformation.Transformer
import org.jetbrains.plugins.scala.lang.transformation.annotations.{AddTypeToValueDefinition, AddTypeToVariableDefinition}
import org.jetbrains.plugins.scala.lang.transformation.calls._

object Converter {
  val transformers: Set[Transformer] = Set(
    new ExpandApplyCall(),
    //    new ExpandUpdateCall(),
    new ImplicitTransform()
  )



  case class ConvertResult(files: Seq[(String, ScalaFile)], definitions: Seq[Definition])

  def convert(files: Seq[ScalaFile], doPrint: Boolean = false): ConvertResult = {
    val convertedFiles = files.map { file =>
      ApplicationManager.getApplication.runWriteAction(new Runnable {
        override def run(): Unit =
          Transformer.transform(file, None, transformers)
      })
      val builder: KotlinBuilder = new KotlinBuilder
      val ast: File = ASTGenerator.gen[File](file)
      if (doPrint)
        println(Utils.prettyPrint(ast))
      val newAst: File = Transform.applyPasses(ast)
      builder.gen(newAst)
      (builder.text, file, newAst.neededDefinitions)
    }
    ConvertResult(
      convertedFiles.map { case (t, f, _) => (t, f) },
      convertedFiles.flatMap(_._3).distinctBy(_.name))
  }
}
