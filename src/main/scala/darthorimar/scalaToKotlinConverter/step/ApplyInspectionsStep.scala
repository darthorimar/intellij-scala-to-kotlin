package darthorimar.scalaToKotlinConverter.step


import java.util

import com.intellij.openapi.components.ServiceManager
import darthorimar.scalaToKotlinConverter.inspection.Inspection
import org.jetbrains.kotlin.caches.resolve.KotlinCacheService
import org.jetbrains.kotlin.psi._
import org.jetbrains.plugins.scala.extensions._

class ApplyInspectionsStep extends ConverterStep[KtElement, KtElement] {
  override def apply(from: KtElement, state: ConverterStepState): (KtElement, ConverterStepState) = {
    val project = from.getProject
    val file = from.getContainingFile.asInstanceOf[KtFile]

    val diagnostics = ServiceManager.getService(project, classOf[KotlinCacheService])
      .getResolutionFacade(util.Collections.singletonList(from))
      .analyzeWithAllCompilerChecks(util.Collections.singletonList(from)).getBindingContext.getDiagnostics

    val fixes = from depthFirst() collect {
      case element: KtElement =>
       Inspection.inspections.flatMap(_.createAction(element, project, file, diagnostics))
    } toList

    fixes.flatten foreach { f =>
      f()
    }

    (from, state)
  }

}
