package darthorimar.scalaToKotlinConverter.inspection

import com.intellij.psi.PsiElement

trait Inspection {
  def isApplicableTo(element: PsiElement) : Boolean
  def applyTo(element: PsiElement): Unit
}
