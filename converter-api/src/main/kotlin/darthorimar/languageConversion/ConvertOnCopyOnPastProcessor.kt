package darthorimar.languageConversion

import com.intellij.codeInsight.editorActions.CopyPastePostProcessor
import com.intellij.codeInsight.editorActions.TextBlockTransferableData
import com.intellij.lang.Language
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.RangeMarker
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Ref
import com.intellij.openapi.util.TextRange
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiFile
import org.jetbrains.kotlin.idea.util.application.executeWriteCommand
import org.jetbrains.kotlin.psi.psiUtil.elementsInRange
import org.jetbrains.kotlin.psi.psiUtil.endOffset
import org.jetbrains.kotlin.psi.psiUtil.startOffset
import java.awt.datatransfer.DataFlavor
import java.awt.datatransfer.Transferable

internal class ConvertOnCopyPastPostProcessor : CopyPastePostProcessor<ConverterTransferableData<*, *>>() {
    override fun collectTransferableData(file: PsiFile?,
                                         editor: Editor?,
                                         startOffsets: IntArray?,
                                         endOffsets: IntArray?):
            List<ConverterTransferableData<*, *>> {
        if (file == null || startOffsets == null || endOffsets == null) return mutableListOf()
        if (startOffsets.size != endOffsets.size) return mutableListOf()
        val converters = LanguageConverterExtension.EP_NAME.extensions

        return converters.mapNotNull { converter ->
            val data = (startOffsets.zip(endOffsets)).map { (from, to) ->
                val range = TextRange(from, to)
                val elementsInRange = file.elementsInRange(range)
                elementsInRange.mapNotNull { convertElement(converter, it) }
            }.flatten()
            if (data.isEmpty()) return@mapNotNull null
            ConverterTransferableData(converter, data)
        }
    }


    private fun <InternalRepresentation, ConverterState> convertElement(
            converter: LanguageConverterExtension<InternalRepresentation, ConverterState>,
            element: PsiElement): ConverterData<InternalRepresentation, ConverterState>? {
        val (internalRepresentation, state) = converter.convertPsiElementToInternalRepresentation(element)
                ?: return null
        if (internalRepresentation == null || state == null) return null
        return ConverterData(element.startOffset,
                element.endOffset,
                internalRepresentation,
                state)
    }

    override fun processTransferableData(project: Project?,
                                         editor: Editor?,
                                         bounds: RangeMarker?,
                                         caretOffset: Int,
                                         indented: Ref<Boolean>?,
                                         values: MutableList<ConverterTransferableData<*, *>>?) {
        if (project == null || values == null || editor == null || bounds == null) return
        val document = editor.document
        val transferebleData = values.single()
        val converter = transferebleData.converter as LanguageConverterExtension<Any, Any>
        if (converter.createOnPasteDialog(project)?.showAndGet() != false) {
            for (data in transferebleData.data) {
                val (text, state) = converter
                        .convertInternalRepresentationToText(data.internalRepresentation as Any,
                                data.state as Any,
                                project) ?: continue
                project.executeWriteCommand("Convert code from ${converter.languageFrom.displayName} to ${converter.languageTo.displayName}", null) {
                    document.replaceString(bounds.startOffset, bounds.endOffset, text)
                    PsiDocumentManager.getInstance(project).commitDocument(document)
                }
                val generatedCodeTextRange = TextRange(bounds.startOffset, bounds.endOffset)
                val psiFile = PsiDocumentManager.getInstance(project).getPsiFile(document) ?: continue
                val generatedElements = psiFile.elementsInRange(generatedCodeTextRange)
                generatedElements.forEach { converter.runPostProcessOperations(it, state) }
            }
        }
    }


    override fun extractTransferableData(content: Transferable?): List<ConverterTransferableData<*, *>> {
        if (content == null) return emptyList()
        val converters = LanguageConverterExtension.EP_NAME.extensions
        return converters.filter {
            content.isDataFlavorSupported(ConverterData.dataFlavor(it.languageFrom))
        }.map { converter ->
            content.getTransferData(ConverterData.dataFlavor(converter.languageFrom)) as ConverterTransferableData<*, *>
        }
    }
}

internal class ConverterTransferableData<InternalRepresentation, ConverterState>(
        val converter: LanguageConverterExtension<InternalRepresentation, ConverterState>,
        val data: List<ConverterData<*, *>>) : TextBlockTransferableData {
    override fun getFlavor(): DataFlavor? = ConverterData.dataFlavor(converter.languageFrom)
    override fun getOffsetCount(): Int = data.size * 2
    override fun getOffsets(offsets: IntArray, index: Int): Int {
        var i: Int = index
        for (d in data) {
            offsets[i] = d.startOffset
            offsets[i + 1] = d.endOffset
            i += 2
        }
        return i
    }

    override fun setOffsets(offsets: IntArray, index: Int): Int {
        var i: Int = index
        for (d in data) {
            d.startOffset = offsets[i]
            d.endOffset = offsets[i + 1]
            i += 2
        }
        return i
    }
}

internal data class ConverterData<InternalRepresentation, ConverterState>(
        var startOffset: Int,
        var endOffset: Int,
        val internalRepresentation: InternalRepresentation,
        val state: ConverterState) {
    companion object {
        fun dataFlavor(toLanguage: Language): DataFlavor? = try {
            val dataClass: Class<*> = this::class.java
            DataFlavor(DataFlavor.javaJVMLocalObjectMimeType + ";class=" + dataClass.name,
                    "Converter${toLanguage.displayName.capitalize()}ReferenceData", dataClass.classLoader)
        } catch (_: Throwable) {
            null
        }
    }
}