package darthorimar.scalaToKotlinConverter.step

import darthorimar.scalaToKotlinConverter.ImplicitTransform
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.transformation.Transformer
import org.jetbrains.plugins.scala.lang.transformation.calls.ExpandApplyCall
import org.jetbrains.plugins.scala.extensions.inWriteAction

class InnerPsiTransformStep extends ConverterStep[ScalaPsiElement, ScalaPsiElement] {
  private val transformers: Set[Transformer] = Set(
    new ExpandApplyCall(),
    new ImplicitTransform()
  )
  override def apply(from: ScalaPsiElement, state: ConverterStepState): (ScalaPsiElement, ConverterStepState) = {
    inWriteAction {
      Transformer.transform(from, None, transformers)
    }
    (from, state)
  }
}
