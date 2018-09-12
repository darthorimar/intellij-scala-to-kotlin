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

/**
 * Add support of converting one language to another.
 *
 * Add actions to menus which names are in [actionGroupNames] which
 * will convert selected or (opened files) from [languageFrom] to [languageTo]
 *
 * Add converting on copy-past from file with language [languageFrom] to one with [languageTo]
 *
 * Perform pos-processing after every conversion by [runPostProcessOperations]
 *
 * [InternalRepresentation] stores data which will be collected by [convertPsiElementToInternalRepresentation] method
 * while copying code
 * [ConverterState] a internal state of convertor which may be used to pass infromation
 * during conversion steps
 *
 * @param languageFrom language to convert from
 * @param languageTo language to convert to
 */
public abstract class LanguageConverterExtension<InternalRepresentation, ConverterState>(val languageFrom: Language,
                                                                                         val languageTo: Language) : AbstractExtensionPointBean() {

    /**
     * Names of IDEA's menu in which convert action will be added
     */
    open val actionGroupNames: Set<String>
        get() = setOf("RefactoringMenu", "EditorTabPopupMenu", "ProjectViewPopupMenu")

    val name
        get() = "${languageFrom.displayName.capitalize()}To${languageTo.displayName.capitalize()}"

    /**
     * Get currently selected files in IDEA
     *
     * By default takes files from project view and currently open file if none of above are selected
     *
     * @param dataContext a [DataContext] stores
     * information about action context it was invoked in
     *
     * @return list of selected files if present, nul otherwise
     */
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

    /**
     * Replace file with language [languageFrom] and current content to
     * a new file with language [languageTo] and [newText] content
     * @param newText text file's content to be replaced to
     * @param file file which should be replaces
     * @param project project where permorm operation
     * @return new file contains code with language [languageTo] if succeed, null otherwise
     */

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

    /**
     * Convert [file] code to a [languageTo] code
     *
     * @param file file which containing code will be converted to [languageTo]
     * @return [Pair] which contains converted code in [languageTo] and [ConverterState] collected if succeed, null otherwise
     */
    abstract fun convertPsiFileToText(file: PsiFile): Pair<String, ConverterState>?

    /**
     * Converts given [element] to [InternalRepresentation]. Called when user performs copy operation
     * Called in dispatch thread with alternative resolving enabled
     *
     * @param element PSI element to convert to [InternalRepresentation]
     * @return [Pair] which contains converted [InternalRepresentation] and collected [ConverterState] if succeed, null otherwise
     */
    abstract fun convertPsiElementToInternalRepresentation(element: PsiElement): Pair<InternalRepresentation, ConverterState>?

    /**
     * Convert given [representation] to code in [languageTo]. Called when user performs past operation
     * Called in write command
     *
     * @param representation [InternalRepresentation] which was collected by [convertPsiElementToInternalRepresentation]
     * @param state [ConverterState] which was collected by [convertPsiElementToInternalRepresentation]
     * @param project project in which converted code will be pasted in
     * @return [Pair] which contains converted code in [languageTo] and collected [ConverterState] if succeed, null otherwise
     */
    abstract fun convertInternalRepresentationToText(representation: InternalRepresentation,
                                                     state: ConverterState,
                                                     project: Project):
            Pair<String, ConverterState>?

    /**
     * Run some post-processing operations like adding imports to file or reformat code
     *
     * @param element PSI element which was converted. May be a [PsiFile] or arbitrary [PsiElement]
     * @param internalState [ConverterState] collected during conversion
     */
    abstract fun runPostProcessOperations(element: PsiElement, internalState: ConverterState)

    companion object {
        val EP_NAME = ExtensionPointName.create<LanguageConverterExtension<*, *>>("com.intellij.languageConverter")
    }
}