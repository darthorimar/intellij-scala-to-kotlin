package darthorimar.languageConversion

import com.intellij.ide.scratch.ScratchFileService
import com.intellij.ide.scratch.ScratchRootType
import com.intellij.openapi.actionSystem.*
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiFile
import com.intellij.psi.PsiManager
import com.intellij.util.containers.isNullOrEmpty
import org.jetbrains.kotlin.idea.refactoring.project

internal class ConvertFileAction<InternalRepresentation, ConverterState>(private val converter: LanguageConverterExtension<InternalRepresentation, ConverterState>) :
        AnAction("Convert ${converter.languageFrom.displayName} to ${converter.languageTo.displayName}") {

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
            val selectedFiles = getSelectedFiles(e.dataContext)
            if (selectedFiles.isNullOrEmpty()) disable() else enable()
        } catch (_: Exception) {
            disable()
        }
    }

    private fun getSelectedFiles(dataContext: DataContext): List<PsiFile>? {
        fun suitableFile(file: PsiFile) =
                file.isWritable && file.language == converter.languageFrom

        val selectedFiles =
                CommonDataKeys.VIRTUAL_FILE_ARRAY.getData(dataContext)?.mapNotNull {
                    PsiManager.getInstance(dataContext.project).findFile(it)
                }?.filter(::suitableFile)
        if (!selectedFiles.isNullOrEmpty()) return selectedFiles
        val inEditorFile =
                CommonDataKeys.PSI_FILE.getData(dataContext)?.takeIf(::suitableFile)
        if (inEditorFile != null) {
            return listOf(inEditorFile)
        }
        return null
    }

    private fun replaceFileContent(newText: String, file: PsiFile, project: Project): PsiFile? {
        val document = PsiDocumentManager.getInstance(project).getDocument(file) ?: return null
        PsiDocumentManager.getInstance(project).doPostponedOperationsAndUnblockDocument(document)
        document.replaceString(0, document.textLength, newText)
        PsiDocumentManager.getInstance(project).commitDocument(document)
        FileDocumentManager.getInstance().saveDocument(document)

        val virtualFile = file.virtualFile
        if (ScratchRootType.getInstance().containsFile(virtualFile)) {
            val mapping = ScratchFileService.getInstance().scratchesMapping
            mapping.setMapping(virtualFile, converter.languageTo)
        } else {
            val fileNameWithoutExtension =
                    file.name.removeSuffix(converter.languageFrom.associatedFileType!!.defaultExtension)
            val newFilename = "$fileNameWithoutExtension${converter.languageTo.associatedFileType!!.defaultExtension}"
            virtualFile.rename(this, newFilename)
        }
        val newDocument = PsiDocumentManager.getInstance(project).getDocument(file)
                ?: return null
        PsiDocumentManager.getInstance(project).commitDocument(newDocument)
        return PsiManager.getInstance(project).findFile(virtualFile)
    }


    override fun actionPerformed(e: AnActionEvent) {
        val project: Project = e.project ?: return
        val selectedFiles: List<PsiFile> = getSelectedFiles(e.dataContext) ?: return
        for (file in selectedFiles) {
            convertFile(file, project)?: showError("Can not convert file ${file.name}", project)
        }
    }

    private fun convertFile(file: PsiFile, project: Project): PsiFile? =
        converter.runConverterCommand(project) {
            val (internalRepresentation, state) =
                    converter.convertPsiElementToInternalRepresentation(file) ?: return@runConverterCommand null
            val (text, newState) =
                    converter.convertInternalRepresentationToText(internalRepresentation, state, project)
                            ?: return@runConverterCommand null
            val newFile = replaceFileContent(text, file, project) ?: return@runConverterCommand null
            converter.runPostProcessOperations(newFile, newState)
            newFile
        }


    companion object {
        const val ACTION_PREFIX = "ConvertLanguageAction"
    }
}