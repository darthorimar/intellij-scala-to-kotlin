package darthorimar.scalaToKotlinConverter.inspection

import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile
import org.jetbrains.kotlin.diagnostics.{Diagnostic, DiagnosticFactory}
import org.jetbrains.kotlin.psi.KtElement
import org.jetbrains.kotlin.resolve.diagnostics.Diagnostics
import collection.JavaConverters._

class DiagnosticBasedInspection(diagnosticFactories: Seq[DiagnosticFactory[_]],
                                fix: (KtElement, Diagnostic, Project, PsiFile) => Unit) extends Inspection {
  override def createAction(element: KtElement,
                            project: Project,
                            file: PsiFile,
                            diagnostics: Diagnostics): Option[Fix] =
    diagnostics.forElement(element).asScala collectFirst {
      case x => x
      case diagnostic: Diagnostic if diagnosticFactories contains diagnostic.getFactory => diagnostic
    } map { diagnostic =>
      val fixAction = () => fix(element, diagnostic, project, file)
      Fix(fixAction, diagnostic)
    }
}