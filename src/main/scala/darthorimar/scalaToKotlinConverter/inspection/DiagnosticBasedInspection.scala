package darthorimar.scalaToKotlinConverter.inspection

import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile
import org.jetbrains.kotlin.diagnostics.{Diagnostic, DiagnosticFactory}
import org.jetbrains.kotlin.psi.KtElement
import org.jetbrains.kotlin.resolve.diagnostics.Diagnostics
import collection.JavaConverters._

class DiagnosticBasedInspection(diagnosticFactories: Seq[DiagnosticFactory[_]],
                                fix: (KtElement, Diagnostic, Project, PsiFile) => Option[() => Unit]) extends Inspection {
  override def createAction(element: KtElement,
                            project: Project,
                            file: PsiFile,
                            diagnostics: Diagnostics): Option[Fix] =
    diagnostics.forElement(element).asScala collectFirst {
      case diagnostic: Diagnostic if diagnosticFactories contains diagnostic.getFactory => diagnostic
    } flatMap { diagnostic =>
      fix(element, diagnostic, project, file) map (Fix(_, diagnostic))
    }
}