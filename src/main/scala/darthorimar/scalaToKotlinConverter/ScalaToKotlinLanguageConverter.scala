package darthorimar.scalaToKotlinConverter

import darthorimar.languageConversion.LanguageConverterExtension
import org.jetbrains.kotlin.psi.KtElement
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement

class ScalaToKotlinLanguageConverter extends LanguageConverterExtension[ScalaPsiElement, KtElement] {
  override def convertPsi(psiElement: ScalaPsiElement): KtElement = {

  }
}
