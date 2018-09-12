package darthorimar.scalaToKotlinConverter

import com.intellij.openapi.project.{Project, ProjectManager}
import com.intellij.psi.{PsiElement, PsiFile}
import darthorimar.languageConversion.LanguageConverterExtension
import darthorimar.scalaToKotlinConverter.ast.AST
import darthorimar.scalaToKotlinConverter.step.ConverterStepState
import org.jetbrains.kotlin.idea.KotlinLanguage
import org.jetbrains.kotlin.psi.KtElement
import org.jetbrains.plugins.scala.ScalaLanguage
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement

class ScalaToKotlinLanguageConverter extends LanguageConverterExtension[AST, ConverterStepState](
  ScalaLanguage.INSTANCE, KotlinLanguage.INSTANCE) {
  override def convertPsiFileToText(file: PsiFile): kotlin.Pair[String, ConverterStepState] =
    file match {
      case scalaElement: ScalaPsiElement =>
        val state = new ConverterStepState
        new ScalaPsiToKotlinTextConverter(file.getProject).convert(scalaElement, state)
      case _ => null
    }

  override def runPostProcessOperations(element: PsiElement, internalState: ConverterStepState): Unit =
    element match {
      case ktElement: KtElement =>
        new PostProcessOperationConverter(element.getProject).convert(ktElement, internalState)
      case _ =>
    }

  override def convertInternalRepresentationToText(representation: AST,
                                                   state: ConverterStepState,
                                                   project: Project): kotlin.Pair[String, ConverterStepState] = {
    new AstToTextConverter(project).convert(representation, state)
  }

  override def convertPsiElementToInternalRepresentation(element: PsiElement): kotlin.Pair[AST, ConverterStepState] =
    element match {
      case scalaElement: ScalaPsiElement =>
        new ScalaPsiToAstConverter(element.getProject).convert(scalaElement, new ConverterStepState)
      case _ => null
    }

  implicit private def scalaPairToKotlinPair[A, B](pair: (A, B)): kotlin.Pair[A, B] =
    new kotlin.Pair(pair._1, pair._2)
}
