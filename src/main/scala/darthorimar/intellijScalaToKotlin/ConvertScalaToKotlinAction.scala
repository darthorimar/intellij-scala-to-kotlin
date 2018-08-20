package darthorimar.intellijScalaToKotlin

import com.intellij.application.options.CodeStyle
import com.intellij.notification.{NotificationDisplayType, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys, LangDataKeys}
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.{VirtualFile, VirtualFileManager}
import com.intellij.psi._
import com.intellij.psi.codeStyle.{CodeStyleManager, CodeStyleSettingsManager}
import darthorimar.intellijScalaToKotlin.Converter.ConvertResult
import darthorimar.intellijScalaToKotlin.definition.{Definition, DefinitionGenerator}
import org.jetbrains.plugins.scala.ScalaLanguage
import org.jetbrains.plugins.scala.conversion.JavaToScala
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

  private def getRootProjectDir(file: ScalaFile): PsiDirectory = {
    val packageParts = file.packageName.split('.')
    if (file.getContainingDirectory.getVirtualFile.getCanonicalPath.endsWith(packageParts.mkString("/"))) {
      Function.chain(Seq.fill(file.packageName.split('.').length)((_: PsiDirectory).getParent))(file.getContainingDirectory)
    } else PsiManager.getInstance(file.getProject).findDirectory(file.getProject.getBaseDir)
  }

  private def createKotlinName(file: ScalaFile): String = {
    val nameWithoutExtension = file.getName.stripSuffix(".scala")
    nameWithoutExtension + ".kt"
  }


  def actionPerformed(e: AnActionEvent) {
    val files = getSelectedFiles(e)
    val (filesToConvert, existingFiles) = files.partition { file =>
      file.getParent.getVirtualFile.findChild(createKotlinName(file)) == null
    }
    existingFiles.foreach { file =>
        NotificationUtil.builder(file.getProject, s"File ${createKotlinName(file)} already exists").
          setDisplayType(NotificationDisplayType.BALLOON)
          .setNotificationType(NotificationType.WARNING)
          .setGroup("convert.scala.to.kotlin")
          .setTitle("Cannot create file")
          .show()
    }
    if (filesToConvert.nonEmpty) {
      ScalaUtils.runWriteAction(() => {
        val ConvertResult(converted, definitions) = Converter.convert(filesToConvert)
        for ((text, file) <- converted) {
          val newName = createKotlinName(file)
          file.getVirtualFile.rename(this, newName)
          val document = PsiDocumentManager.getInstance(file.getProject).getDocument(file)
          document.deleteString(0, document.getTextLength)
          document.insertString(0, text)
          PsiDocumentManager.getInstance(file.getProject).commitDocument(document)
          val kotlinFile = PsiDocumentManager.getInstance(file.getProject).getPsiFile(document)
          Utils.reformatFile(kotlinFile)
        }

        DefinitionGenerator.generate(definitions, getRootProjectDir(files.head))
      }, files.head.getProject, "Convert to Kotlin")
    }
  }

}
