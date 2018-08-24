package darthorimar.scalaToKotlinConverter

import java.awt.datatransfer.{DataFlavor, Transferable}
import java.{lang, util}

import com.intellij.codeInsight.editorActions.{CopyPastePostProcessor, TextBlockTransferableData}
import com.intellij.openapi.editor.{Editor, RangeMarker}
import com.intellij.psi.{PsiDocumentManager, PsiElement, PsiFile, PsiManager}
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import java.util.Collections

import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Ref
import darthorimar.scalaToKotlinConverter.ast._
import darthorimar.scalaToKotlinConverter.definition.DefinitionGenerator
import darthorimar.scalaToKotlinConverter.step.{ApplyInspectionsStep, ConverterStepState}
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.plugins.hocon.CommonUtil.TextRange
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement

import collection.JavaConverters._

class ConvertOnCopyPastPostProcessor extends CopyPastePostProcessor[ScalaToKotlinTransferableData] {
  override def collectTransferableData(file: PsiFile,
                                       editor: Editor,
                                       startOffsets: Array[Int],
                                       endOffsets: Array[Int]): util.List[ScalaToKotlinTransferableData] =
    file match {
      case scalaFile: ScalaFile =>
        val data =
          inWriteAction {
            (startOffsets zip endOffsets) map {
              case (from, to) =>
                val range = TextRange.apply(from, to)
                val psiInRange =
                  scalaFile depthFirst() filter { element =>
                    range contains element.getTextRange
                  } maxBy (_.getTextRange.getLength)
                val (ast, state) = Converter
                  .scalaPsiToAst(psiInRange.asInstanceOf[ScalaPsiElement], new ConverterStepState)
                new ScalaToKotlinData(from, to, ast, state)
            }
          }

        Collections.singletonList(new ScalaToKotlinTransferableData(data))
      case _ =>
        Collections.emptyList[ScalaToKotlinTransferableData]
    }

  override def extractTransferableData(content: Transferable): util.List[ScalaToKotlinTransferableData] = {
    if (content.isDataFlavorSupported(ScalaToKotlinData.dataFlavor))
      Collections.singletonList(
        content.getTransferData(ScalaToKotlinData.dataFlavor).asInstanceOf[ScalaToKotlinTransferableData])
    else null
  }

  override def processTransferableData(project: Project,
                                       editor: Editor,
                                       bounds: RangeMarker,
                                       caretOffset: Int,
                                       indented: Ref[lang.Boolean],
                                       values: util.List[ScalaToKotlinTransferableData]): Unit =
    PsiDocumentManager.getInstance(project).getPsiFile(bounds.getDocument) match {
      case ktFile: KtFile =>
        inWriteAction {
          values.asScala foreach { transData =>
            transData.data foreach { data =>
              val ast = data.ast
              val oldState = data.state
              val document = editor.getDocument
              val (text, newState) = Converter.astToKotlinText(ast, oldState)
              DefinitionGenerator.generate(newState.collectedDefinitions, Utils.getSrcDir(ktFile))
              document.replaceString(bounds.getStartOffset, bounds.getEndOffset, text)
              PsiDocumentManager.getInstance(project).commitDocument(document)
              Utils.addImportsToKtFile(ktFile, newState.collectImports)
              new ApplyInspectionsStep().apply(ktFile, oldState)
            }
          }
        }

      case _ =>
    }
}

class ScalaToKotlinTransferableData(val data: Array[ScalaToKotlinData]) extends TextBlockTransferableData {
  override def getFlavor: DataFlavor = ScalaToKotlinData.dataFlavor

  override def getOffsetCount: Int = data.length * 2

  override def getOffsets(offsets: Array[Int], index: Int): Int = {
    var i = index
    for (d <- data) {
      offsets(i) = d.startOffset
      offsets(i + 1) = d.endOffset
      i += 2
    }
    i
  }

  override def setOffsets(offsets: Array[Int], index: Int): Int = {
    var i = index
    for (d <- data) {
      d.startOffset = offsets(i)
      d.endOffset = offsets(i + 1)
      i += 2
    }
    i
  }

}

case class ScalaToKotlinData(var startOffset: Int = 0,
                             var endOffset: Int = 0,
                             ast: AST,
                             state: ConverterStepState)

object ScalaToKotlinData {
  lazy val dataFlavor: DataFlavor =
    try {
      val dataClass = ScalaToKotlinData.getClass
      new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType + ";class=" + dataClass.getName,
        "KotlinReferenceData",
        dataClass.getClassLoader)
    }
    catch {
      case _: Throwable => null
    }
}

