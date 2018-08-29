package darthorimar.scalaToKotlinConverter.inspection

import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile
import org.jetbrains.kotlin.diagnostics.{Diagnostic, Errors}
import org.jetbrains.kotlin.idea.inspections.{ExplicitThisInspection, KotlinDoubleNegationInspection}
import org.jetbrains.kotlin.idea.intentions.{MoveLambdaInsideParenthesesIntention, RemoveUnnecessaryParenthesesIntention, SelfTargetingOffsetIndependentIntention, UsePropertyAccessSyntaxIntention}
import org.jetbrains.kotlin.idea.quickfix.AddExclExclCallFix
import org.jetbrains.kotlin.psi.{KtCallExpression, KtElement, KtLambdaArgument, KtParenthesizedExpression}
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
        if (fix.isAvailable(project, null, file)) Some(() => fix.invoke(project, null, file))
        else None
    }),
    new IntentionBaseInspection(new RemoveUnnecessaryParenthesesIntention, classOf[KtParenthesizedExpression]),
    new IntentionBaseInspection(new UsePropertyAccessSyntaxIntention, classOf[KtCallExpression]),
    new IntentionBaseInspection(new MoveLambdaInsideParenthesesIntention, classOf[KtLambdaArgument])
  )
}