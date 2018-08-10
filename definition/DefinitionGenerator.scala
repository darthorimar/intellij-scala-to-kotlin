package org.jetbrains.plugins.kotlinConverter.definition

import com.intellij.psi.{PsiDirectory, PsiDocumentManager}
import org.jetbrains.plugins.kotlinConverter.Utils
import org.jetbrains.plugins.scala.extensions._

import scala.io.Source


object DefinitionGenerator {
  def generate(definitions: Seq[Definition], directory: PsiDirectory): Unit = {
    val definitionsGenerated =
      definitions
        .distinctBy(_.name)
        .map {
          case FileDefinition(_, file) =>
            //todo use resource path
            Source.fromFile(s"/home/ilya/code/intellij-scala/scala/scala-impl/resources/org/jetbrains/plugins/kotlinConverter/definition/$file")
              .getLines()
              .mkString("\n")
          case textDef: TextDefinition =>
            textDef.get
        }
    val text = definitionsGenerated.mkString("\n\n")
    createFile("lib.kt", directory, text)
  }

  private def createFile(name: String, directory: PsiDirectory, text: String): Unit = {
    val file = directory.createFile(name)
    val document = PsiDocumentManager.getInstance(file.getProject).getDocument(file)
    document.insertString(0, text)
    Utils.reformatFile(file)
  }
}
