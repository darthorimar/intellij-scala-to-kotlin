package darthorimar.scalaToKotlinConverter

import darthorimar.scalaToKotlinConverter.ast.AST
import darthorimar.scalaToKotlinConverter.step.PrintKotlinCodeStep.KotlinCode
import darthorimar.scalaToKotlinConverter.step.transform._
import darthorimar.scalaToKotlinConverter.step._
import org.jetbrains.kotlin.psi.KtElement
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement

object Converter {
  val scalaPsiToAst: ConverterStep[ScalaPsiElement, AST] =
    new InnerPsiTransformStep -->
      new ASTGenerationStep


  val astToKotlinCode: ConverterStep[AST, KotlinCode] =
    new TypeTransform -->
      new BasicTransform -->
      new CollectionTransform -->
      new TypeTransform -->
      new DefinitionCollectorTransform -->
      new CollectImportsStep -->
      new PrintKotlinCodeStep

  val kotlinCodeToKotlinPsi: ConverterStep[KotlinCode, KtElement] =
    new GenerateKtElementStep -->
      new ApplyInspectionsStep


  val scalaPsiToKotlinCode: ConverterStep[ScalaPsiElement, KotlinCode] =
    scalaPsiToAst -->
      astToKotlinCode


  val scalaPsiToKotlinPsi: ConverterStep[ScalaPsiElement, KtElement] =
    scalaPsiToKotlinCode -->
      kotlinCodeToKotlinPsi

  val astToKotlinPsi: ConverterStep[AST, KtElement] =
    astToKotlinCode -->
      kotlinCodeToKotlinPsi
}
