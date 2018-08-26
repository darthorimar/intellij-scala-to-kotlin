package darthorimar.scalaToKotlinConverter.step


import darthorimar.scalaToKotlinConverter.inspection.DefaultInspection
import darthorimar.scalaToKotlinConverter.step.ApplyInspectionsStep._
import org.jetbrains.kotlin.idea.inspections.{ExplicitThisInspection, KotlinDoubleNegationInspection}
import org.jetbrains.kotlin.psi._
import org.jetbrains.plugins.scala.extensions._

class ApplyInspectionsStep extends ConverterStep[KtElement, KtElement] {
  override def apply(from: KtElement, state: ConverterStepState): (KtElement, ConverterStepState) = {
    from depthFirst() foreach { element =>
      inspections foreach { inspection =>
        inspection(element)
      }
    }
    (from, state)
  }
}


object ApplyInspectionsStep {
  val inspections = Seq(
    new DefaultInspection(new ExplicitThisInspection),
    new DefaultInspection(new KotlinDoubleNegationInspection)
  )
}