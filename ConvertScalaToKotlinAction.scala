package org.jetbrains.plugins.kotlinConverter

import com.intellij.application.options.CodeStyle
import com.intellij.notification.{NotificationDisplayType, NotificationType}
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys, LangDataKeys}
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.{PsiDocumentManager, PsiJavaFile}
import com.intellij.psi.codeStyle.{CodeStyleManager, CodeStyleSettingsManager}
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
      val elements = getElements(e)
      val enabled =
        elements.foldLeft(true) {
          case (acc, s: ScalaFile) => acc && s.getContainingDirectory.isWritable
          case _ => false
        }
      if (enabled) enable()
      else disable()
    }
    catch {
      case _: Exception => disable()
    }

  }

  def getElements(e: AnActionEvent) =
    Option(LangDataKeys.PSI_ELEMENT_ARRAY.getData(e.getDataContext)).
      orElse {
        Option(CommonDataKeys.PSI_FILE.getData(e.getDataContext)).map(e => Array(e))
      }.getOrElse(Array.empty)


  def actionPerformed(e: AnActionEvent) {
    val elements = getElements(e)
    for (element <- elements) {
      element.getContainingFile match {
        case jFile: ScalaFile =>
          val dir = jFile.getContainingDirectory
          if (dir.isWritable) {
            ScalaUtils.runWriteAction(() => {
              val directory = jFile.getContainingDirectory
              val name = jFile.getName.stripSuffix(".scala")
              val nameWithExtension = name + ".kt"
              val existingFile = Option(directory.getVirtualFile.findChild(nameWithExtension))
              if (existingFile.isDefined) {
                NotificationUtil.builder(directory.getProject, s"File $nameWithExtension already exists")
                  .setDisplayType(NotificationDisplayType.BALLOON)
                  .setNotificationType(NotificationType.ERROR)
                  .setGroup("rename.scala.to.kotlin")
                  .setTitle("Cannot create file")
                  .show()
              } else {
                val file = directory.createFile(nameWithExtension)
                val newText = Converter.convert(jFile).trim
                val document = PsiDocumentManager.getInstance(file.getProject).getDocument(file)
                document.insertString(0, newText)
                PsiDocumentManager.getInstance(file.getProject).commitDocument(document)
                val manager: CodeStyleManager = CodeStyleManager.getInstance(file.getProject)
                manager.reformatText(file, 0, file.getTextLength)
                jFile.delete()
                file.navigate(true)
              }
            }, jFile.getProject, "Convert to Kotlin")
          }
        case _ =>
      }
    }
  }


}
