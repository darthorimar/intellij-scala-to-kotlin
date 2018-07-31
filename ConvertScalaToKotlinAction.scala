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
      var elements = LangDataKeys.PSI_ELEMENT_ARRAY.getData(e.getDataContext)
      if (elements == null) {
        val file = CommonDataKeys.PSI_FILE.getData(e.getDataContext)
        if (file != null) elements = Array(file)
        else elements = Array.empty
      }
      for (element <- elements) {
        element.getContainingFile match {
          case j: ScalaFile =>
            val dir = j.getContainingDirectory
            if (!dir.isWritable) {
              disable()
              return
            }
          case _ =>
            disable()
            return
        }
      }
      enable()
    }
    catch {
      case _: Exception => disable()
    }

  }


  def actionPerformed(e: AnActionEvent) {
    var elements = LangDataKeys.PSI_ELEMENT_ARRAY.getData(e.getDataContext)
    if (elements == null) {
      val file = CommonDataKeys.PSI_FILE.getData(e.getDataContext)
      if (file != null) elements = Array(file)
      else elements = Array.empty
    }
    for (element <- elements) {
      element.getContainingFile match {
        case jFile: ScalaFile =>
          val dir = jFile.getContainingDirectory
          if (dir.isWritable) {
            ScalaUtils.runWriteAction(new Runnable {
              def run() {
                val directory = jFile.getContainingDirectory
                val name = jFile.getName.substring(0, jFile.getName.length - 5)
                val nameWithExtension: String = name + "kt"
                val existingFile: VirtualFile = directory.getVirtualFile.findChild(nameWithExtension)
                if (existingFile != null) {
                  NotificationUtil.builder(directory.getProject, s"File $nameWithExtension already exists").
                    setDisplayType(NotificationDisplayType.BALLOON).
                    setNotificationType(NotificationType.ERROR).
                    setGroup("rename.scala.to.kotlin").
                    setTitle("Cannot create file").
                    show()
                  return
                }
                val file = directory.createFile(name + "kt")
                val newText = Converter.convert(jFile).trim
                val document = PsiDocumentManager.getInstance(file.getProject).getDocument(file)
                document.insertString(0, newText)
                PsiDocumentManager.getInstance(file.getProject).commitDocument(document)
                val manager: CodeStyleManager = CodeStyleManager.getInstance(file.getProject)
                manager.reformatText(file, 0, file.getTextLength)
                file.navigate(true)
              }
            }, jFile.getProject, "Convert to Kotlin")
          }
        case _ =>
      }
    }
  }


}
