package darthorimar.languageConversion

import com.intellij.notification.NotificationDisplayType
import com.intellij.notification.NotificationType
import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.actionSystem.Presentation
import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile
import com.intellij.util.containers.isNullOrEmpty
import org.jetbrains.kotlin.idea.util.application.executeWriteCommand
import org.jetbrains.plugins.scala.util.NotificationUtil

class ConvertFileAction<InternalRepresentation, ConverterState>(private val converter: LanguageConverterExtension<InternalRepresentation, ConverterState>) :
        AnAction(converter.title) {

    override fun update(e: AnActionEvent) {
        val presentation: Presentation = e.presentation
        fun enable() {
            presentation.isEnabled = true
            presentation.isVisible = true
        }

        fun disable() {
            presentation.isEnabled = false
            presentation.isVisible = false
        }
        try {
            val selectedFiles = converter.getSelectedFiles(e.dataContext)
            if (selectedFiles.isNullOrEmpty()) disable() else enable()
        } catch (_: Exception) {
            disable()
        }
    }

    override fun actionPerformed(e: AnActionEvent) {
        val project: Project = e.project!!
        val selectedFiles: List<PsiFile> = converter.getSelectedFiles(e.dataContext) ?: return
        for (file in selectedFiles) {
            val result = convertFile(file, project)
            if (result == null) {
                showError("Can not convert file ${file.name}", project)
                continue
            }
            converter.runPostProcessOperations(result.first, result.second)
        }
    }

    private fun convertFile(file: PsiFile, project: Project): Pair<PsiFile, ConverterState>? =
            project.executeWriteCommand("Convert file from ${converter.languageFrom.displayName} to ${converter.languageTo.displayName}", null) {
                CommandProcessor.getInstance().markCurrentCommandAsGlobal(project)
                val (text, state) = converter.convertPsiFileToText(file) ?: return@executeWriteCommand null
                val newFile = converter.replaceFileContent(text, file, project)
                newFile?.let { it to state }
            }

    private fun showError(message: String, project: Project) {
        NotificationUtil.builder(project, message)
                .setDisplayType(NotificationDisplayType.BALLOON)
                .setNotificationType(NotificationType.WARNING)
                .setGroup("language.converter")
                .setTitle("Cannot convert file").show()
    }

    companion object {
        const val ACTION_PREFIX = "ConvertLanguageAction."
    }
}