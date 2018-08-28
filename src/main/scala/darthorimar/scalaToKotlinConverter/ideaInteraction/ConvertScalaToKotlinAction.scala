package darthorimar.scalaToKotlinConverter.ideaInteraction

import com.intellij.ide.scratch.{ScratchFileService, ScratchRootType}
import com.intellij.notification.{NotificationDisplayType, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.editor.Document
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.ex.MessagesEx
import com.intellij.psi._
import darthorimar.scalaToKotlinConverter.Converter.ConvertResult
import darthorimar.scalaToKotlinConverter.{Converter, Utils}
import darthorimar.scalaToKotlinConverter.definition.DefinitionGenerator
import darthorimar.scalaToKotlinConverter.step.ApplyInspectionsStep
import org.jetbrains.kotlin.idea.KotlinFileType
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.util.{NotificationUtil, ScalaUtils}

class ConvertScalaToKotlinAction extends AnAction {

  override def update(e: AnActionEvent) {
    val presentation = e.getPresentation

    def enable() {
      presentation.setEnabled(true)
      presentation.setVisible(true)
    }

    def disable() {
      presentation.setEnabled(false)
      presentation.setVisible(false)
    }

    try {
      val elements = getSelectedFiles(e)
      if (elements.nonEmpty) enable()
      else disable()
    }
    catch {
      case _: Exception => disable()
    }

  }

  def getSelectedFiles(e: AnActionEvent): Seq[ScalaFile] =
    Option(CommonDataKeys.VIRTUAL_FILE_ARRAY.getData(e.getDataContext))
      .map(_.toSeq.flatMap { virtualFile =>
        Option(PsiManager.getInstance(e.getProject).findFile(virtualFile))
      }).
      orElse {
        Option(CommonDataKeys.PSI_FILE.getData(e.getDataContext)).map(Seq(_))
      }.getOrElse(Seq.empty)
      .collect {
        case file: ScalaFile if file.getContainingDirectory.isWritable => file
      }

  private def createKotlinName(file: ScalaFile): String = {
    val nameWithoutExtension = file.getName.stripSuffix(".scala")
    nameWithoutExtension + ".kt"
  }


  def actionPerformed(e: AnActionEvent) {
    val project = e.getProject
    val files = getSelectedFiles(e)
    val (filesToConvert, existingFiles) = files.partition { file =>
      file.getParent.getVirtualFile.findChild(createKotlinName(file)) == null
    }
    existingFiles.foreach { file =>
      NotificationUtil.builder(project, s"File ${createKotlinName(file)} already exists").
        setDisplayType(NotificationDisplayType.BALLOON)
        .setNotificationType(NotificationType.WARNING)
        .setGroup("convert.scala.to.kotlin")
        .setTitle("Cannot create file")
        .show()
    }
    if (filesToConvert.nonEmpty) {
      project.executeRunCommand("Convert Scala to Kotlin") {
        val ConvertResult(converted) = Converter.convert(filesToConvert)
        for ((text, file: ScalaFile, state) <- converted) {
          val kotlinFile = replaceFileText(file, project, text)
          Utils.reformatFile(kotlinFile)
          val imports = state.collectImports
          Utils.addImportsToKtFile(kotlinFile, imports)
          PsiDocumentManager.getInstance(project).commitAllDocuments()
          new ApplyInspectionsStep().apply(kotlinFile, state)
        }
        val definitions = converted.flatMap(_._3.collectedDefinitions)
        DefinitionGenerator
          .generate(definitions, Utils.getSrcDir(files.head))
      }
    }
  }

  def replaceFileText(file: ScalaFile, project: Project, newText: String): KtFile = {
    val document = PsiDocumentManager.getInstance(project).getDocument(file)
    PsiDocumentManager.getInstance(project).doPostponedOperationsAndUnblockDocument(document)
    document.replaceString(0, document.getTextLength, newText)
    PsiDocumentManager.getInstance(project).commitDocument(document)
    FileDocumentManager.getInstance().saveDocument(document)

    val virtualFile = file.getVirtualFile
    if (ScratchRootType.getInstance().containsFile(virtualFile)) {
      val mapping = ScratchFileService.getInstance().getScratchesMapping
      mapping.setMapping(virtualFile, KotlinFileType.INSTANCE.getLanguage)
    }
    else {
      val fileName = createKotlinName(file)
      virtualFile.rename(this, fileName)
    }
    PsiManager.getInstance(project).findFile(virtualFile).asInstanceOf[KtFile]
  }

}