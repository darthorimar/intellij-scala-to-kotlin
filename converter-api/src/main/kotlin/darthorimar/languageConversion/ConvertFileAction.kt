package darthorimar.languageConversion

import com.intellij.ide.scratch.ScratchFileService
import com.intellij.ide.scratch.ScratchRootType
import com.intellij.notification.NotificationDisplayType
import com.intellij.notification.NotificationType
import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.actionSystem.Presentation
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiFile
import com.intellij.psi.PsiManager
import com.intellij.util.containers.isNullOrEmpty
import org.jetbrains.kotlin.idea.util.application.runWriteAction
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.util.NotificationUtil
import org.jetbrains.plugins.scala.util.NotificationUtil.NotificationBuilder

class ConvertFileAction<InternalState>(private val converter: LanguageConverterExtension<InternalState>) : AnAction() {
    companion object {
        const val ACTION_PREFIX = "ConvertLanguageAction."
    }

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

    override fun actionPerformed(e: AnActionEvent): Unit {
        val project: Project = e.project!!
        val selectedFiles: List<PsiFile> = converter.getSelectedFiles(e.dataContext) ?: return
        for (file in selectedFiles) {
            val result = convertFile(file, project)
            if (result == null) {
                showError("Can not convert file ${file.name}", project)
                continue
            }
            converter.runPosProcessOperation(result.first, result.second)
        }
    }

    private fun convertFile(file: PsiFile, project: Project): Pair<PsiFile, InternalState>? =
            runWriteAction {
                val (text, state) = converter.convertPsiFile(file)
                val document = PsiDocumentManager.getInstance(project).getDocument(file) ?: return@runWriteAction null
                PsiDocumentManager.getInstance(project).doPostponedOperationsAndUnblockDocument(document)
                document.replaceString(0, document.textLength, text)
                PsiDocumentManager.getInstance(project).commitDocument(document)
                FileDocumentManager.getInstance().saveDocument(document)

                val virtualFile = file.virtualFile
                if (ScratchRootType.getInstance().containsFile(virtualFile)) {
                    val mapping = ScratchFileService.getInstance().scratchesMapping
                    mapping.setMapping(virtualFile, converter.languageTo)
                } else {
                    val fileNameWithoutExtension =
                            file.name.removeSuffix(converter.languageFrom.associatedFileType!!.defaultExtension)
                    val newFilename = "$fileNameWithoutExtension.${converter.languageTo.associatedFileType!!.defaultExtension}"
                    virtualFile.rename(this, newFilename)
                }
                val newDocument = PsiDocumentManager.getInstance(project).getDocument(file)
                        ?: return@runWriteAction null
                PsiDocumentManager.getInstance(project).commitDocument(newDocument)
                PsiManager.getInstance(project).findFile(virtualFile)?.let { it to state }
            }

    fun showError(message: String, project: Project) {
        NotificationUtil.builder(project, message)
                .setDisplayType(NotificationDisplayType.BALLOON)
                .setNotificationType(NotificationType.WARNING)
                .setGroup("language.converter")
                .setTitle("Cannot convert file").show()
    }
}