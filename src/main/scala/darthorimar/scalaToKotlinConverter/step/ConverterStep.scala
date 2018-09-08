package darthorimar.scalaToKotlinConverter.step

import ConverterStep._

trait ConverterStep[From, To] {
  self =>

  def apply(from: From, state: ConverterStepState): Result[To]

  def -->[NextTo](nextStep: ConverterStep[To, NextTo]): ConverterStep[From, NextTo] =
    (from: From, state: ConverterStepState) => {
      val (result, newState) = self.apply(from, state)
      nextStep.apply(result, newState)
    }
}

object ConverterStep {
  type Result[T] = (T, ConverterStepState)
}