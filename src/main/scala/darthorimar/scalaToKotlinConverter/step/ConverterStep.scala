package darthorimar.scalaToKotlinConverter.step

import ConverterStep._

trait ConverterStep[From, To] {
  self =>
  def name: String

  def apply(from: From,
            state: ConverterStepState,
            index: Int,
            notifier: Notifier): Result[To]

  def -->[NextTo](nextStep: ConverterStep[To, NextTo]): ConverterStep[From, NextTo] =
    new ConverterStep[From, NextTo] {
      override def apply(from: From,
                         state: ConverterStepState,
                         index: Int,
                         notifier: Notifier): Result[NextTo] = {
        val (result, newState) = self(from, state, index, notifier)
        nextStep(result, newState, index + 1, notifier)
      }

      override def name: String = nextStep.name
    }
}

object ConverterStep {
  type Result[T] = (T, ConverterStepState)

  trait Notifier {
    def notify(step: ConverterStep[_, _], index: Int)
  }
  object Notifier {
    val empty: Notifier = (step: ConverterStep[_, _], index: Int) => {}
  }

}