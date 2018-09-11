package darthorimar.languageConversion

import com.intellij.lang.Language
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.actionSystem.CommonDataKeys
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.actionSystem.DataKey
import com.intellij.openapi.extensions.AbstractExtensionPointBean
import com.intellij.openapi.extensions.ExtensionPointName
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import com.intellij.psi.PsiManager
import com.intellij.util.containers.isNullOrEmpty
import org.jetbrains.kotlin.idea.refactoring.project

public abstract class LanguageConverterExtension<InternalState>(public val languageFrom: Language,
                                                                public val languageTo: Language) : AbstractExtensionPointBean() {

    val name
        get() = "${languageFrom.displayName.capitalize()}To${languageTo.displayName.capitalize()}"

    abstract fun convertPsiFile(psiFile: PsiFile): Pair<String, InternalState>

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

    abstract fun runPosProcessOperation(file: PsiFile, internalState: InternalState)


    //   abstract fun createIntermediateRepresentation(psiElement: PsiFrom): Intermediate
    //   abstract fun createPsiFromIntermediateRepresentation(intermediate: Intermediate): PsiTo
    companion object {
        val EP_NAME = ExtensionPointName.create<LanguageConverterExtension>("com.intellij.languageConverter")
    }
}