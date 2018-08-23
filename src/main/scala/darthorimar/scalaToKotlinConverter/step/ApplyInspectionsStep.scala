package darthorimar.scalaToKotlinConverter.step

import com.intellij.codeInspection.{InspectionManager, LocalQuickFixOnPsiElement, ProblemDescriptor, ProblemsHolder}
import org.jetbrains.kotlin.idea.inspections.ConstantConditionIfInspection
import org.jetbrains.kotlin.idea.intentions.ConvertToStringTemplateInspection
import org.jetbrains.kotlin.psi.{KtElement, KtFile}
import org.jetbrains.plugins.scala.codeInsight.intention.RemoveBracesIntention
import org.jetbrains.plugins.scala.extensions._

class ApplyInspectionsStep extends ConverterStep[KtElement, KtElement] {
  override def apply(from: KtElement, state: ConverterStepState): (KtElement, ConverterStepState) = {
    val project = from.getProject
    val file = from.getContainingFile

    val holder = new ProblemsHolder(InspectionManager.getInstance(project), file, false)

    val inspectionVisitor = new ConstantConditionIfInspection().buildVisitor(holder, false)
    from depthFirst() foreach {
      case psi: KtElement =>
        inspectionVisitor.visitElement(psi)
      case _ =>
    }
    holder.getResults.forEach { problem =>
      problem.getFixes collect {
        case f: LocalQuickFixOnPsiElement => f
      } foreach (_.applyFix)
    }
    (from, state)
  }


}
