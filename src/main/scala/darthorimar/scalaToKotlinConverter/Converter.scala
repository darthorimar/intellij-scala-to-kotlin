package darthorimar.scalaToKotlinConverter

import darthorimar.scalaToKotlinConverter.ast.AST
import darthorimar.scalaToKotlinConverter.step.transform._
import darthorimar.scalaToKotlinConverter.step._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement

object Converter {

  case class ConvertResult(files: Seq[(String, ScalaPsiElement, ConverterStepState)])

  def convert(psis: Seq[ScalaPsiElement], doPrint: Boolean = false): ConvertResult = {
    val convertedFiles =
      psis map { psi =>
        val (text, state) = scalaPsiToKotlinText(psi, new ConverterStepState)
        (text, psi, state)
      }
    ConvertResult(convertedFiles)
  }

  val scalaPsiToAst: ConverterStep[ScalaPsiElement, AST] =
    new InnerPsiTransformStep -->
      new ASTGenerationStep


  val astToKotlinText: ConverterStep[AST, String] =
    new TypeTransform -->
      new BasicTransform -->
      new CollectionTransform -->
      new TypeTransform -->
      new CollectorTransform -->
      new RefCollectorTransform -->
      new KotlinBuilderStep


  val scalaPsiToKotlinText: ConverterStep[ScalaPsiElement, String] =
    scalaPsiToAst -->
      astToKotlinText
}
