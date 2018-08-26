package darthorimar.scalaToKotlinConverter.ideaInteraction

import com.intellij.notification.{NotificationDisplayType, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.editor.Document
import com.intellij.openapi.project.Project
import com.intellij.psi._
import darthorimar.scalaToKotlinConverter.Converter.ConvertResult
import darthorimar.scalaToKotlinConverter.{Converter, Utils}
import darthorimar.scalaToKotlinConverter.definition.DefinitionGenerator
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
      ScalaUtils.runWriteAction(() => {
        val ConvertResult(converted) = Converter.convert(filesToConvert)
        for ((text, file: ScalaFile, state) <- converted) {
          val newName = createKotlinName(file)
          file.getVirtualFile.rename(this, newName)
          val document = PsiDocumentManager.getInstance(project).getDocument(file)
          replaceFileText(document, project, text)
          val kotlinFile = PsiDocumentManager.getInstance(project).getPsiFile(document).asInstanceOf[KtFile]
          Utils.reformatFile(kotlinFile)
          val imports = state.collectImports
          Utils.addImportsToKtFile(kotlinFile, imports)
        }
        val definitions = converted.flatMap(_._3.collectedDefinitions)
        DefinitionGenerator
          .generate(definitions, Utils.getSrcDir(files.head))
      }, files.head.getProject, "Convert to Kotlin")
    }
  }

  def replaceFileText(document: Document, project: Project, newText: String): Unit = {
    document.deleteString(0, document.getTextLength)
    document.insertString(0, newText)
    PsiDocumentManager.getInstance(project).commitDocument(document)
  }

}