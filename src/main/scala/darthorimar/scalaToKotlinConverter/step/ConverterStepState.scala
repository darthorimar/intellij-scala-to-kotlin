package darthorimar.scalaToKotlinConverter.step

import com.intellij.ide.scratch.{ScratchFileService, ScratchRootType}
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiDocumentManager, PsiManager}
import darthorimar.scalaToKotlinConverter.Utils
import darthorimar.scalaToKotlinConverter.ast.Import
import darthorimar.scalaToKotlinConverter.definition.{Definition, DefinitionGenerator, FileDefinition}
import org.jetbrains.kotlin.idea.KotlinFileType
import org.jetbrains.kotlin.psi.{KtElement, KtFile}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.extensions._

trait KtElementGenerator {
  def insertCode(text: String): KtElement
}

class FileElementGenerator(file: ScalaFile) extends KtElementGenerator {
  override def insertCode(text: String): KtElement = inWriteAction {
    val project = file.getProject
    val document = PsiDocumentManager.getInstance(project).getDocument(file)
    PsiDocumentManager.getInstance(project).doPostponedOperationsAndUnblockDocument(document)
    document.replaceString(0, document.getTextLength, text)
    PsiDocumentManager.getInstance(project).commitDocument(document)
    FileDocumentManager.getInstance().saveDocument(document)

    val virtualFile = file.getVirtualFile
    if (ScratchRootType.getInstance().containsFile(virtualFile)) {
      val mapping = ScratchFileService.getInstance().getScratchesMapping
      mapping.setMapping(virtualFile, KotlinFileType.INSTANCE.getLanguage)
    }
    else {
      val fileName = Utils.createKotlinName(file)
      virtualFile.rename(this, fileName)
    }
    val ktDocument = PsiDocumentManager.getInstance(project).getDocument(file)
    PsiDocumentManager.getInstance(project).commitDocument(ktDocument)

    PsiManager.getInstance(project).findFile(virtualFile).asInstanceOf[KtFile]
  }
}

class ConverterStepState(var elementGenerator: Option[KtElementGenerator] = None) {
  private var definitions: Set[Definition] = Set.empty
  private var imports: Set[Import] = Set.empty

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