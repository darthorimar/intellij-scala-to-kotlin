package darthorimar.languageConversion

import com.intellij.notification.NotificationDisplayType
import com.intellij.notification.NotificationType
import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.project.Project
import org.jetbrains.kotlin.idea.util.application.executeWriteCommand
import org.jetbrains.plugins.scala.util.NotificationUtil

internal fun <T> LanguageConverterExtension<*, *>.runConverterCommand(project: Project, command: () -> T): T =
        project.executeWriteCommand(
                "Convert file from ${this.languageFrom.displayName} to ${this.languageTo.displayName}", null) {
            CommandProcessor.getInstance().markCurrentCommandAsGlobal(project)
            command()
        }

internal fun showError(message: String, project: Project) {
    NotificationUtil.builder(project, message)
            .setDisplayType(NotificationDisplayType.BALLOON)
            .setNotificationType(NotificationType.WARNING)
            .setGroup("language.converter")
            .setTitle("Error while converting code").show()
}