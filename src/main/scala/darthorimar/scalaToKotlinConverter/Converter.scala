package darthorimar.scalaToKotlinConverter

import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.{Computable, ThrowableComputable}
import darthorimar.scalaToKotlinConverter.ast.AST
import darthorimar.scalaToKotlinConverter.step.ConverterStep.Result
import darthorimar.scalaToKotlinConverter.step.PrintKotlinCodeStep.KotlinCode
import darthorimar.scalaToKotlinConverter.step.transform._
import darthorimar.scalaToKotlinConverter.step._
import org.jetbrains.kotlin.psi.KtElement
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement
import org.jetbrains.plugins.scala.util.ScalaUtils
import org.jetbrains.plugins.scala.extensions._

class Converter(project: Project) {
  val scalaPsiToAst: ConverterStep[ScalaPsiElement, AST] =
  //    new InnerPsiTransformStep -->
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
    wrapped[ScalaPsiElement, KotlinCode](withProgress,
      new ASTGenerationStep -->
        new TypeTransform -->
        new BasicTransform -->
        new CollectionTransform -->
        new TypeTransform -->
        new DefinitionCollectorTransform -->
        new CollectImportsStep -->
        new PrintKotlinCodeStep
    ) -->
      wrapped(inWriteCommand,
        new GenerateKtElementStep -->
          new ApplyInspectionsStep
      )


  def wrapped[From, To](wrapper: (=>Result[To]) => Result[To],
                        step: ConverterStep[From, To]): ConverterStep[From, To] =
    (from: From, state: ConverterStepState) => {
      wrapper(step(from, state))
    }

  def withProgress[T](data: => T): T = {
    val convert: ThrowableComputable[T, Exception] =
      () =>
        ProgressManager.getInstance().runProcess(
          (() => data): Computable[T], ProgressManager.getInstance().getProgressIndicator)
    ProgressManager.getInstance()
      .runProcessWithProgressSynchronously(convert, title, true, project)
  }

  def inWriteCommand[T](data: => T): T = {
    var result: T = null.asInstanceOf[T]
    CommandProcessor.getInstance()
      .executeCommand(project, () => {
        CommandProcessor.getInstance().markCurrentCommandAsGlobal(project)
        result = inWriteAction(data)
      }, title, null)
    result
  }

  val title = "Converting Scala to Kotlin"
}

