package org.jetbrains.plugins.kotlinConverter

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys, LangDataKeys}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile

class ConvertScalaToKotlinAction extends AnAction {

  override def update(e: AnActionEvent): Unit = {
    val presentation = e.getPresentation

    def enable(): Unit = {
      presentation.setEnabled(true)
      presentation.setVisible(true)
    }

    def disable(): Unit = {
      presentation.setEnabled(false)
      presentation.setVisible(false)
    }

    val elements =
      Option(LangDataKeys.PSI_ELEMENT_ARRAY.getData(e.getDataContext))
        .orElse {
          Option(CommonDataKeys.PSI_FILE.getData(e.getDataContext)).map(Array(_))
        }.getOrElse(Array.empty)
    val toggle =
      elements
        .map(_.getContainingFile)
        .exists {
          case j: ScalaFile => j.getContainingDirectory.isWritable
          case _ => false
        }
    if (toggle) enable()
    else        disable()


  }

  override def actionPerformed(anActionEvent: AnActionEvent): Unit = {

  }
}
