package darthorimar.scalaToKotlinConverter.inspection

import com.intellij.psi.PsiElement

trait Inspection {
  def apply(element: PsiElement): Unit
}
