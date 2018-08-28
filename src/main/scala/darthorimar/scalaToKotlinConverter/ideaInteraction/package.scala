package darthorimar.scalaToKotlinConverter

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.project.Project
import org.jetbrains.plugins.scala.extensions._

package object ideaInteraction {

  implicit class ProjectOps(project: Project) {
    def executeCommand[T](name: String, command: => T): T = {
      var result: T = null.asInstanceOf[T]
      CommandProcessor.getInstance()
        .executeCommand(project, () => result = command, name, null)
      result
    }

    def executeRunCommand[T](name: String)(command: => T): T =
      executeCommand(name, inWriteAction(command))
  }

}
