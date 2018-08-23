package darthorimar.scalaToKotlinConverter.step

trait ConverterStep[From, To] {
  self =>

  def apply(from: From, state: ConverterStepState): (To, ConverterStepState)

  def -->[NextTo](nextStep: ConverterStep[To, NextTo]): ConverterStep[From, NextTo] =
    (from: From, state: ConverterStepState) => {
      val (result, newState) = self(from, state)
      nextStep(result, newState)
    }
}
