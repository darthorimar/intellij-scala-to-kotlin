package darthorimar.intellijScalaToKotlin

import com.intellij.application.options.CodeStyle
import com.intellij.notification.{NotificationDisplayType, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys, LangDataKeys}
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
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
      val enabled =
        elements.forall(s => s.getContainingDirectory.isWritable)
      if (enabled) enable()
      else disable()
    }
    catch {
      case _: Exception => disable()
    }

  }

  def getSelectedFiles(e: AnActionEvent): Seq[ScalaFile] =
    Option(LangDataKeys.PSI_ELEMENT_ARRAY.getData(e.getDataContext)).
      orElse {
        Option(CommonDataKeys.PSI_FILE.getData(e.getDataContext)).map(e => Array(e))
      }.getOrElse(Array.empty)
      .collect {
        case f: ScalaFile if f.getContainingDirectory.isWritable => f
      }.toSeq

  private def getRootProjectDir(file: ScalaFile): PsiDirectory = {
    val packageParts = file.packageName.split('.')
    if (file.getContainingDirectory.getVirtualFile.getCanonicalPath.endsWith(packageParts.mkString("/"))) {
      Function.chain(Seq.fill(file.packageName.split('.').length)((_: PsiDirectory).getParent))(file.getContainingDirectory)
    } else PsiManager.getInstance(file.getProject).findDirectory(file.getProject.getBaseDir)
  }

  def actionPerformed(e: AnActionEvent) {
    val files = getSelectedFiles(e)
    if (files.nonEmpty) {
      ScalaUtils.runWriteAction(() => {
        val ConvertResult(converted, definitions) = Converter.convert(files)
        for ((text, file) <- converted) {
          val nameWithoutExtension = file.getName.stripSuffix(".scala")
          val newName = nameWithoutExtension + ".kt"
          file.getVirtualFile.rename(this, newName)
          val document = PsiDocumentManager.getInstance(file.getProject).getDocument(file)
          document.deleteString(0, document.getTextLength)
          document.insertString(0, text)
          PsiDocumentManager.getInstance(file.getProject).commitDocument(document)
          val kotlinFile = PsiDocumentManager.getInstance(file.getProject).getPsiFile(document)
          Utils.reformatFile(kotlinFile)
          file.getProject.getBaseDir.getChildren
        }

        DefinitionGenerator.generate(definitions, getRootProjectDir(files.head))
      }, files.head.getProject, "Convert to Kotlin")
    }
  }

}
