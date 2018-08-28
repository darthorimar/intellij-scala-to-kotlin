package darthorimar.scalaToKotlinConverter.step


import java.util

import com.intellij.openapi.components.ServiceManager
import darthorimar.scalaToKotlinConverter.inspection.{Fix, Inspection}
import org.jetbrains.kotlin.caches.resolve.KotlinCacheService
import org.jetbrains.kotlin.diagnostics.Errors
import org.jetbrains.kotlin.psi._
import org.jetbrains.plugins.scala.extensions._

import collection.JavaConverters._

class ApplyInspectionsStep extends ConverterStep[KtElement, KtElement] {
  override def apply(from: KtElement, state: ConverterStepState): (KtElement, ConverterStepState) = {
    val project = from.getProject
    val file = from.getContainingFile.asInstanceOf[KtFile]


    var fixes: List[Fix] = List.empty
    do {
      val diagnostics = inReadAction {
        ServiceManager.getService(project, classOf[KotlinCacheService])
          .getResolutionFacade(util.Collections.singletonList(from))
          .analyzeWithAllCompilerChecks(util.Collections.singletonList(from)).getBindingContext.getDiagnostics
      }
      fixes = from depthFirst() flatMap {
        case element: KtElement =>
          Inspection.inspections.flatMap(_.createAction(element, project, file, diagnostics))
        case _ => List.empty
      } toList

      fixes foreach { f =>
        inWriteAction(f())
      }
    } while (fixes.nonEmpty)

    (from, state)
  }

}
