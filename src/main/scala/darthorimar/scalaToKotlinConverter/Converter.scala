package darthorimar.scalaToKotlinConverter

import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.progress.{ProgressIndicator, ProgressManager, Task}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.{Computable, ThrowableComputable}
import com.intellij.util.Alarm
import com.intellij.util.concurrency.Semaphore
import darthorimar.scalaToKotlinConverter.ast.AST
import darthorimar.scalaToKotlinConverter.step.ConverterStep.{Notifier, Result}
import darthorimar.scalaToKotlinConverter.step.PrintKotlinCodeStep.KotlinCode
import darthorimar.scalaToKotlinConverter.step.transform._
import darthorimar.scalaToKotlinConverter.step._
import org.jetbrains.kotlin.psi.KtElement
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement
import org.jetbrains.plugins.scala.util.ScalaUtils
import org.jetbrains.plugins.scala.extensions._

class Converter(project: Project) {

  def convertScalaPsiToKotlinPsi(from: ScalaPsiElement, state: ConverterStepState): KtElement = {
    val stepsCount = 10
    val notifier = new Notifier {
      override def notify(step: ConverterStep[_, _], index: Int): Unit = {
        Option(ProgressManager.getInstance().getProgressIndicator) foreach { indicator =>
          indicator.setFraction(index.toDouble / stepsCount)
          val filename = from.getContainingFile.getName
          indicator.setText(s"Converting file $filename")
          indicator.setText2(s"${step.name} $index/$stepsCount")
        }
      }
    }
    val (result, _) = scalaPsiToKotlinPsi(from, state, 0, notifier)
    result
  }


  val scalaPsiToKotlinPsi: ConverterStep[ScalaPsiElement, KtElement] =
    wrapped[ScalaPsiElement, ScalaPsiElement](inWriteCommand,
      wrapped(withProgress(background = false),
        new InnerPsiTransformStep
      )
    ) -->
      wrapped[ScalaPsiElement, KotlinCode](withProgress(background = true),
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
        wrapped(withProgress(background = false),
          new GenerateKtElementStep -->
            new ApplyInspectionsStep
        )
      )


  def wrapped[From, To](wrapper: (=> Result[To]) => Result[To],
                        step: ConverterStep[From, To]): ConverterStep[From, To] = new ConverterStep[From, To] {
    override def apply(from: From, state: ConverterStepState, index: Int, notifier: Notifier): (To, ConverterStepState) = {
      wrapper(step(from, state, index, notifier))
    }

    override def name: String = step.name
  }

  def withProgress[T](background: Boolean)(data: => T): T = {
    val convert: ThrowableComputable[T, Exception] = () =>
      ProgressManager.getInstance().runProcess(
        (() => data): Computable[T], ProgressManager.getInstance().getProgressIndicator)


    if (background)
      ProgressManager.getInstance()
        .runProcessWithProgressSynchronously(convert, title, true, project)
    else {
      var result: T = null.asInstanceOf[T]
      val task = new Task.Modal(project, title, false) {
        override def run(indicator: ProgressIndicator): Unit =
          result = convert.compute()
      }
      ProgressManager.getInstance().run(task)
      result
    }
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

