package darthorimar.scalaToKotlinConverter.inspection

import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.kotlin.idea.intentions.{SelfTargetingIntention, SelfTargetingRangeIntention}
import org.jetbrains.kotlin.psi.KtElement
import org.jetbrains.kotlin.resolve.diagnostics.Diagnostics

class IntentionBaseInspection[Elem <: KtElement](intension: SelfTargetingIntention[Elem],
                                                 elementType: Class[Elem]) extends Inspection {


  override def createAction(element: KtElement,
                            project: Project,
                            file: PsiFile,
                            diagnostics: Diagnostics): Option[Fix] = {

    def stillAvailable(): Boolean = intension match {
      case int: SelfTargetingRangeIntention[Elem] =>
        int.applicabilityRange(element.asInstanceOf[Elem]) != null
      case int: SelfTargetingIntention[Elem] =>
        int.isApplicableTo(element.asInstanceOf[Elem], element.getTextRange.getStartOffset)
    }

    val action = () =>
      if (element.getClass == elementType && stillAvailable())
        intension.applyTo(element.asInstanceOf[Elem], null)

    if (element.getClass == elementType && stillAvailable())
      Some(Fix(action))
    else None
  }
}
