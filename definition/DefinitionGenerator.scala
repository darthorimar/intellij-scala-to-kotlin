package org.jetbrains.plugins.kotlinConverter.definition

import com.intellij.psi.{PsiDirectory, PsiDocumentManager}
import org.jetbrains.plugins.kotlinConverter.Utils
import org.jetbrains.plugins.scala.extensions._

import scala.collection.mutable
import scala.io.Source


object DefinitionGenerator {
  private def collectDefinitions(definitions: Seq[Definition]): Seq[Definition] = {
    val visited = mutable.Map.empty[String, Definition]
    var stack: List[Definition] = definitions.toList
    while (stack.nonEmpty) {
      val definition = {
        val head = stack.head
        stack = stack.tail
        head
      }
      if (!visited.contains(definition.name)) {
        visited += definition.name -> definition
        for (dependency <- definition.dependencies) {
          stack = dependency :: stack
        }
      }
    }
    visited.values.toSeq
  }

  def generate(definitions: Seq[Definition], directory: PsiDirectory): Unit = {
    val definitionsGenerated =
      collectDefinitions(definitions)
        .map {
          case d: FileDefinition =>
            val filename = d.filename
            //todo use resource path
            Source.fromFile(s"/home/ilya/code/intellij-scala/scala/scala-impl/resources/org/jetbrains/plugins/kotlinConverter/definition/$filename")
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
