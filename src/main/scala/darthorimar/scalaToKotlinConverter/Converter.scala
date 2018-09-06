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


  val astToKotlinPsi: ConverterStep[AST, KtElement] =
    new TypeTransform -->
      new BasicTransform -->
      new CollectionTransform -->
      new TypeTransform -->
      new DefinitionCollectorTransform -->
      new CollectImportsStep -->
      new PrintKotlinCodeStep -->
      new GenerateKtElementStep -->
      new ApplyInspectionsStep


  val scalaPsiToKotlinPsi: ConverterStep[ScalaPsiElement, KtElement] =
    scalaPsiToAst -->
      astToKotlinPsi
}
