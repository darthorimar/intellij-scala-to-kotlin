package darthorimar.scalaToKotlinConverter.ideaInteraction

import com.intellij.ide.scratch.{ScratchFileService, ScratchRootType}
import com.intellij.notification.{NotificationDisplayType, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.editor.Document
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.ex.MessagesEx
import com.intellij.psi._
import darthorimar.scalaToKotlinConverter.{Converter, Utils}
import darthorimar.scalaToKotlinConverter.definition.DefinitionGenerator
import darthorimar.scalaToKotlinConverter.step.PrintKotlinCodeStep.KotlinCode
import darthorimar.scalaToKotlinConverter.step.{ApplyInspectionsStep, ConverterStepState, FileElementGenerator, KtElementGenerator}
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
    Option(CommonDataKeys.VIRTUAL_FILE_ARRAY.getData(e.getDataContext)) map {
      _.toList flatMap { virtualFile =>
        Option(PsiManager.getInstance(e.getProject).findFile(virtualFile))
      }
    } orElse {
      Option(CommonDataKeys.PSI_FILE.getData(e.getDataContext)).map(Seq(_))
    } getOrElse Seq.empty collect {
      case file: ScalaFile if file.getContainingDirectory.isWritable => file
    }


  def actionPerformed(e: AnActionEvent) {
    val project = e.getProject
    val files = getSelectedFiles(e)
    val (filesToConvert, existingFiles) = files.partition { file =>
      file.getParent.getVirtualFile.findChild(Utils.createKotlinName(file)) == null
    }
    existingFiles foreach { file =>
      NotificationUtil.builder(project, s"File ${Utils.createKotlinName(file)} already exists").
        setDisplayType(NotificationDisplayType.BALLOON)
        .setNotificationType(NotificationType.WARNING)
        .setGroup("convert.scala.to.kotlin")
        .setTitle("Cannot create file")
        .show()
    }

    if (filesToConvert.nonEmpty) {
      project.executeRunCommand("Convert Scala to Kotlin") {
        filesToConvert foreach { file =>
          val state = new ConverterStepState
          state.elementGenerator = Some(new FileElementGenerator(file))
          Converter.scalaPsiToKotlinPsi(file, state)
        }
        PsiDocumentManager.getInstance(project).commitAllDocuments()
      }
    }
  }
}