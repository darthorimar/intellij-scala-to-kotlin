package darthorimar.scalaToKotlinConverter.step

import com.intellij.codeInspection._
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiElement, PsiElementVisitor, PsiFile}
import com.siyeh.ig.style.UnnecessaryParenthesesInspection
import org.jetbrains.kotlin.idea.inspections.{AbstractKotlinInspection, ConstantConditionIfInspection, ExplicitThisInspection}
import org.jetbrains.kotlin.idea.intentions.{ConvertToStringTemplateInspection, SelfTargetingIntention}
import org.jetbrains.kotlin.idea.quickfix.QuickFixActionBase
import org.jetbrains.kotlin.psi._
import org.jetbrains.plugins.scala.codeInsight.intention.RemoveBracesIntention
import org.jetbrains.plugins.scala.extensions._
import darthorimar.scalaToKotlinConverter._
import darthorimar.scalaToKotlinConverter.step.ApplyInspectionsStep._

import scala.collection.mutable

class ApplyInspectionsStep extends ConverterStep[KtElement, KtElement] {

  private def applySmartFix[D <: CommonProblemDescriptor](fix: QuickFix[D], descriptor: D, project: Project): Unit = {
    (descriptor, fix) match {
      case (problemDescriptor: ProblemDescriptor, intentionFix: IntentionWrapper) =>
        def applySelfTargetingIntention(action: SelfTargetingIntention[PsiElement]): Unit = {
          val target =
            action.getTargetByOffset(
              problemDescriptor.getPsiElement.getTextRange.getStartOffset,
              problemDescriptor.getPsiElement.getContainingFile)
          if (target == null) return
          if (!action.isApplicableTo(target, problemDescriptor.getPsiElement.getTextRange.getStartOffset)) return
          action.applyTo(target, null)
        }

        def applyQuickFixActionBase(action: QuickFixActionBase[PsiElement]) {
          if (!action.isAvailable(project, null, problemDescriptor.getPsiElement.getContainingFile)) return
          action.invoke(project, null, problemDescriptor.getPsiElement.getContainingFile)
        }

        intentionFix.getAction match {
          case action: SelfTargetingIntention[PsiElement] => applySelfTargetingIntention(action)
          case action: QuickFixActionBase[PsiElement] => applyQuickFixActionBase(action)
          case _ =>
        }

      case _ =>
    }

    fix.applyFix(project, descriptor)
  }

  override def apply(from: KtElement, state: ConverterStepState): (KtElement, ConverterStepState) = {
    val project = from.getProject
    val file = from.getContainingFile

    val holder = new ProblemsHolder(InspectionManager.getInstance(project), file, false)

    val visitors = inspections map {
      _.buildVisitor(holder, false)
    }
    from depthFirst() foreach { element =>
      visitors foreach { visitor =>
        element.accept(visitor)
      }
    }

    holder.getResults.forEach { descriptor =>
      descriptor.getFixes collect {
        case f: LocalQuickFix => f
      } foreach (applySmartFix(_, descriptor, project))
    }

    (from, state)
  }
}


object ApplyInspectionsStep {
  val inspections =  Seq(
    new UnnecessaryParenthesesInspection
  )
  implicit class SelfTargetingIntentionOps(val intention: SelfTargetingIntention[PsiElement]) {
    def getTargetByOffset(offset: Int, file: PsiFile): PsiElement = {
      val leaf1 = file.findElementAt(offset)
      val leaf2 = file.findElementAt(offset - 1)
      val commonParent = if (leaf1 != null && leaf2 != null) PsiTreeUtil.findCommonParent(leaf1, leaf2) else null

      val elementsToCheck = mutable.ListBuffer.empty[PsiElement]

      def parentsWithSelf(psi: PsiElement) =
        Stream.iterate(psi)(_.getParent) takeWhile (!_.isInstanceOf[PsiFile])

      if (leaf1 != null) {
        elementsToCheck.appendAll(parentsWithSelf(leaf1)
          .takeWhile(_ != commonParent))
      }
      if (leaf2 != null) {
        elementsToCheck.appendAll(parentsWithSelf(leaf2)
          .takeWhile(_ != commonParent))
      }
      if (commonParent != null && !commonParent.isInstanceOf[PsiFile]) {
        elementsToCheck.appendAll(parentsWithSelf(commonParent))
      }

      def containsInside(range: TextRange, i: Int) =
        range.getStartOffset < offset && offset < range.getEndOffset

      import scala.util.control.Breaks._
      breakable {
        for (element <- elementsToCheck) {
          if (intention.isApplicableTo(element.asInstanceOf[PsiElement], offset)) {
            return element.asInstanceOf[PsiElement]
          }
          if (element.isInstanceOf[KtBlockExpression] && containsInside(element.getTextRange, offset)) break
        }
      }
      null
    }

  }

}