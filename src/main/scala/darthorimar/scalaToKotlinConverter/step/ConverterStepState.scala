package darthorimar.scalaToKotlinConverter.step

import com.intellij.ide.scratch.{ ScratchFileService, ScratchRootType }
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.project.Project
import com.intellij.psi.{ PsiDocumentManager, PsiManager }
import darthorimar.scalaToKotlinConverter.Utils
import darthorimar.scalaToKotlinConverter.ast.Import
import darthorimar.scalaToKotlinConverter.definition.{ Definition, DefinitionGenerator, FileDefinition }
import org.jetbrains.kotlin.idea.KotlinFileType
import org.jetbrains.kotlin.psi.{ KtElement, KtFile }
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.extensions._

class ConverterStepState {
  private var definitions: Set[Definition] = Set.empty
  private var imports: Set[Import]         = Set.empty

  def addDefinition(definition: Definition): Unit = {
    val definitionNames = definition match {
      case fileDefinition: FileDefinition =>
        fileDefinition.usedDefinitions
      case _ => Seq(definition.name)
    }
    definitionNames foreach { definitionName =>
      imports += Import(DefinitionGenerator.packageName + "." + definitionName)
    }
    definitions += definition
  }

  def addImport(imp: Import): Unit =
    imports += imp

  def collectedDefinitions: Seq[Definition] =
    definitions.toSeq

  def collectImports: Seq[Import] =
    imports.toSeq
}
