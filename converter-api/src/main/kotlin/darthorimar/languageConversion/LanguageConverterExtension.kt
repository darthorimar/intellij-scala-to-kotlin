package darthorimar.languageConversion

import com.intellij.ide.scratch.ScratchFileService
import com.intellij.ide.scratch.ScratchRootType
import com.intellij.lang.Language
import com.intellij.openapi.actionSystem.*
import com.intellij.openapi.extensions.AbstractExtensionPointBean
import com.intellij.openapi.extensions.ExtensionPointName
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.PsiManager
import com.intellij.util.containers.isNullOrEmpty
import org.jetbrains.kotlin.idea.refactoring.project

public abstract class LanguageConverterExtension<InternalRepresentation, ConverterState>(val languageFrom: Language,
                                                                                         val languageTo: Language) : AbstractExtensionPointBean() {

    open val actionGroupNames: Set<String>
        get() = setOf("RefactoringMenu", "EditorTabPopupMenu", "ProjectViewPopupMenu")

    val name
        get() = "${languageFrom.displayName.capitalize()}To${languageTo.displayName.capitalize()}"


    open fun getSelectedFiles(dataContext: DataContext): List<PsiFile>? {
        fun suitableFile(file: PsiFile) =
                file.isWritable && file.language == languageFrom

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

    open fun replaceFileContent(newText: String, file: PsiFile, project: Project): PsiFile? {
        val document = PsiDocumentManager.getInstance(project).getDocument(file) ?: return null
        PsiDocumentManager.getInstance(project).doPostponedOperationsAndUnblockDocument(document)
        document.replaceString(0, document.textLength, newText)
        PsiDocumentManager.getInstance(project).commitDocument(document)
        FileDocumentManager.getInstance().saveDocument(document)

        val virtualFile = file.virtualFile
        if (ScratchRootType.getInstance().containsFile(virtualFile)) {
            val mapping = ScratchFileService.getInstance().scratchesMapping
            mapping.setMapping(virtualFile, languageTo)
        } else {
            val fileNameWithoutExtension =
                    file.name.removeSuffix(languageFrom.associatedFileType!!.defaultExtension)
            val newFilename = "$fileNameWithoutExtension.${languageTo.associatedFileType!!.defaultExtension}"
            virtualFile.rename(this, newFilename)
        }
        val newDocument = PsiDocumentManager.getInstance(project).getDocument(file)
                ?: return null
        PsiDocumentManager.getInstance(project).commitDocument(newDocument)
        return PsiManager.getInstance(project).findFile(virtualFile)
    }

    abstract fun convertPsiFileToText(file: PsiFile): Pair<String, ConverterState>?

    abstract fun convertPsiElementToInternalRepresentation(element: PsiElement): Pair<InternalRepresentation, ConverterState>?

    abstract fun convertInternalRepresentationToText(representation: InternalRepresentation,
                                                     state: ConverterState,
                                                     project: Project):
            Pair<String, ConverterState>?

    abstract fun runPostProcessOperations(element: PsiElement, internalState: ConverterState)

    companion object {
        val EP_NAME = ExtensionPointName.create<LanguageConverterExtension<*, *>>("com.intellij.languageConverter")
    }
}