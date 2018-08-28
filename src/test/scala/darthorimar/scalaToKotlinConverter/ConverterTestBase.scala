package darthorimar.scalaToKotlinConverter

import com.intellij.testFramework.LightPlatformTestCase
import darthorimar.scalaToKotlinConverter.step.ConverterStepState
import org.jetbrains.kotlin.psi.KtPsiFactory
import org.jetbrains.plugins.scala.base.ScalaLightPlatformCodeInsightTestCaseAdapter
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.junit.Assert._

abstract class ConverterTestBase extends ScalaLightPlatformCodeInsightTestCaseAdapter {

  private def formatKotlinCode(unformatedCode: String, filterImports: Boolean): String = {
    val ktPsiFactory = new KtPsiFactory(LightPlatformTestCase.getProject)
    val ktFile = ktPsiFactory.createFile(unformatedCode)
    Utils.reformatKtElement(ktFile)
    val formated = ktFile.getText.trim.split('\n').filterNot(_.isEmpty).mkString("\n")
    if (filterImports)
      formated.split('\n').filterNot(_.startsWith("import")).mkString("\n")
    else formated
  }

  def doTest(scala: String,
             kotlin: String,
             filterImports: Boolean = false): Unit = {
    configureFromFileTextAdapter("dummy.scala", scala)
    val psiFile = getFileAdapter

    val (generatedKotlinCode, _) = Converter.scalaPsiToKotlinCode(psiFile.asInstanceOf[ScalaFile], new ConverterStepState)
    val formatedExpected = formatKotlinCode(kotlin, filterImports = false)
    val formatedActual = formatKotlinCode(generatedKotlinCode, filterImports)
    assertEquals(formatedExpected, formatedActual)
  }

  def doExprTest(scala: String, kotlin: String): Unit = {
    doTest(s"def a = {$scala \n 42}",
      s"fun a(): Int {$kotlin \n return 42}",
      filterImports = true)
  }

}
