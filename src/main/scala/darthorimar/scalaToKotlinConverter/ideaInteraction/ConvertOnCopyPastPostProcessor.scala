package darthorimar.scalaToKotlinConverter.ideaInteraction

import java.awt.datatransfer.{DataFlavor, Transferable}
import java.util.Collections
import java.{lang, util}

import com.intellij.codeInsight.editorActions.{CopyPastePostProcessor, TextBlockTransferableData}
import com.intellij.openapi.editor.{Editor, RangeMarker}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.{Ref, TextRange}
import com.intellij.psi.{PsiDocumentManager, PsiElement, PsiFile}
import darthorimar.scalaToKotlinConverter._
import darthorimar.scalaToKotlinConverter.ast._
import darthorimar.scalaToKotlinConverter.definition.DefinitionGenerator
import darthorimar.scalaToKotlinConverter.step.{ConverterStepState, KtElementGenerator}
import org.jetbrains.kotlin.psi.{KtElement, KtFile}
import org.jetbrains.plugins.hocon.CommonUtil.TextRange
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile

import scala.collection.JavaConverters._
import scala.util.Try

class ConvertOnCopyPastPostProcessor extends CopyPastePostProcessor[ScalaToKotlinTransferableData] {

  private def getPsiInRange(file: PsiFile, range: TextRange): PsiElement =
    file depthFirst() filter { element =>
      range contains element.getTextRange
    } maxBy (_.getTextRange.getLength)


  override def collectTransferableData(file: PsiFile,
                                       editor: Editor,
                                       startOffsets: Array[Int],
                                       endOffsets: Array[Int]): util.List[ScalaToKotlinTransferableData] =
    Try {
      file match {
        case scalaFile: ScalaFile =>
          val data =
            inWriteAction {
              (startOffsets zip endOffsets) flatMap {
                case (from, to) =>
                  getPsiInRange(scalaFile, new TextRange(from, to)) match {
                    case scalaPsi: ScalaPsiElement =>
                      val (ast, state) =
                        new ScalaPsiToAstConverter(scalaFile.getProject).convert(scalaPsi, new ConverterStepState)
                      val data = new ScalaToKotlinData(from, to, ast, state)
                      Seq(data)
                    case _ => Seq.empty
                  }
              }
            }

          Collections.singletonList(new ScalaToKotlinTransferableData(data))
        case _ =>
          Collections.emptyList[ScalaToKotlinTransferableData]
      }
    } getOrElse Collections.emptyList[ScalaToKotlinTransferableData]

  override def extractTransferableData(content: Transferable): util.List[ScalaToKotlinTransferableData] = {
    if (content.isDataFlavorSupported(ScalaToKotlinData.dataFlavor))
      Collections.singletonList(
        content.getTransferData(ScalaToKotlinData.dataFlavor).asInstanceOf[ScalaToKotlinTransferableData])
    else Collections.emptyList[ScalaToKotlinTransferableData]
  }

  override def processTransferableData(project: Project,
                                       editor: Editor,
                                       bounds: RangeMarker,
                                       caretOffset: Int,
                                       indented: Ref[lang.Boolean],
                                       values: util.List[ScalaToKotlinTransferableData]): Unit =
    PsiDocumentManager.getInstance(project).getPsiFile(bounds.getDocument) match {
      case ktFile: KtFile =>
        if (new ConvertScalaToKotlinDialog(project).showAndGet()) {
          values.asScala foreach { transData =>
            transData.data foreach { data =>
              val ast = data.ast
              val state = data.state
              val document = editor.getDocument

              val ktElementGenerator: KtElementGenerator =
                (code: String) => {
                  document.replaceString(bounds.getStartOffset, bounds.getEndOffset, code)
                  PsiDocumentManager.getInstance(project).commitDocument(document)
                  val generatedCodeTextRange = new TextRange(bounds.getStartOffset, bounds.getStartOffset + code.length)
                  getPsiInRange(ktFile, generatedCodeTextRange).asInstanceOf[KtElement]
                }
              state.elementGenerator = Some(ktElementGenerator)
              new AstToKotlinPsiConverter(project).convert(ast, state)
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

