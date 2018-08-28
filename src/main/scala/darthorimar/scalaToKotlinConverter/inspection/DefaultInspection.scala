package darthorimar.scalaToKotlinConverter.inspection

import com.intellij.codeInspection._
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi.{PsiElement, PsiFile}
import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.kotlin.idea.inspections.AbstractKotlinInspection
import org.jetbrains.kotlin.idea.intentions.SelfTargetingIntention
import org.jetbrains.kotlin.idea.quickfix.QuickFixActionBase
import org.jetbrains.kotlin.psi.{KtBlockExpression, KtElement}

import scala.collection.mutable
import darthorimar.scalaToKotlinConverter.inspection.DefaultInspection._
import org.jetbrains.kotlin.resolve.diagnostics.Diagnostics
import collection.JavaConverters._

class DefaultInspection(inspection: AbstractKotlinInspection) extends Inspection {
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

  override def createAction(element: KtElement,
                            project: Project,
                            file: PsiFile,
                            diagnostics: Diagnostics): Option[Fix] = {
    val holder = new ProblemsHolder(InspectionManager.getInstance(project), file, false)
    val visitor = inspection.buildVisitor(holder, false)
    element.accept(visitor)
    val actions = holder.getResults.asScala flatMap { descriptor =>
      descriptor.getFixes collectFirst {
        case f: LocalQuickFix => f
      } map {
        fix => () => applySmartFix(fix, descriptor, project)
      }
    }
    if (actions.isEmpty) None
    else {
      val fixAction = actions.reduce((acc, f) => () => { acc(); f() })
      Some(Fix(fixAction, null))
    }
  }
}

object DefaultInspection {

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
            return element
          }
          if (element.isInstanceOf[KtBlockExpression] && containsInside(element.getTextRange, offset)) break
        }
      }
      null
    }

  }

}