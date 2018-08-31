package darthorimar.scalaToKotlinConverter.inspection

import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile
import org.jetbrains.kotlin.idea.inspections.AbstractApplicabilityBasedInspection
import org.jetbrains.kotlin.psi.KtElement
import org.jetbrains.kotlin.resolve.diagnostics.Diagnostics

class ApplicabilityBasedInspection[Elem <: KtElement](val inspection: AbstractApplicabilityBasedInspection[Elem],
                                                      elementType: Class[Elem]) extends Inspection {
  override def createAction(element: KtElement,
                            project: Project,
                            file: PsiFile,
                            diagnostics: Diagnostics): Option[Fix] = {
    def isStillAvalable: Boolean =
      element.getClass == elementType && inspection.isApplicable(element.asInstanceOf[Elem])

    if (isStillAvalable) Some(Fix(() => if (isStillAvalable) inspection.applyTo(element, project, null)))
    else None
  }

}
