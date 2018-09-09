package darthorimar.scalaToKotlinConverter.step

import darthorimar.scalaToKotlinConverter.ImplicitTransform
import darthorimar.scalaToKotlinConverter.step.ConverterStep.Notifier
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.transformation.Transformer
import org.jetbrains.plugins.scala.lang.transformation.calls.ExpandApplyCall
import org.jetbrains.plugins.scala.extensions.inWriteAction

class InnerPsiTransformStep extends ConverterStep[ScalaPsiElement, ScalaPsiElement] {
  override def name: String = "Preparing code for translation"

  private val transformers: Set[Transformer] = Set(
    new ImplicitTransform()
  )
  override def apply(from: ScalaPsiElement,
                     state: ConverterStepState,
                     index: Int,
                     notifier: Notifier): (ScalaPsiElement, ConverterStepState) = {
    notifier.notify(this, index)
    val result = inWriteAction {
      val copy  = from.copy()
      Transformer.transform(copy, None, transformers)
      copy.asInstanceOf[ScalaPsiElement]
    }
    (result, state)
  }
}
