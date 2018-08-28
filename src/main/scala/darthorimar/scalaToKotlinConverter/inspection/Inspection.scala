package darthorimar.scalaToKotlinConverter.inspection

import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile
import org.jetbrains.kotlin.diagnostics.{Diagnostic, Errors}
import org.jetbrains.kotlin.idea.inspections.{ExplicitThisInspection, KotlinDoubleNegationInspection}
import org.jetbrains.kotlin.idea.quickfix.AddExclExclCallFix
import org.jetbrains.kotlin.psi.KtElement
import org.jetbrains.kotlin.resolve.diagnostics.Diagnostics
import collection.JavaConverters._

trait Inspection {
  def createAction(element: KtElement, project: Project, file: PsiFile, diagnostics: Diagnostics): Option[Fix]
}

case class Fix(fix: () => Unit, diagnostic: Diagnostic) {
  def apply(): Unit = fix()
}

object Inspection {
  val inspections = Seq(
    new DiagnosticBasedInspection(Errors.TYPE_MISMATCH_ERRORS.asScala.toSeq, {
      (element: KtElement, diagnostic: Diagnostic, project: Project, file: PsiFile) =>
        val fix = new AddExclExclCallFix(element)
        fix.invoke(project, null, file)
    })
//    ,
//    new DefaultInspection(new ExplicitThisInspection),
//    new DefaultInspection(new KotlinDoubleNegationInspection)
  )
}