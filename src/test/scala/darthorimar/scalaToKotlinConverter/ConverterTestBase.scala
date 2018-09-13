package darthorimar.scalaToKotlinConverter

import com.intellij.openapi.command.CommandProcessor
import com.intellij.psi.PsiFileFactory
import com.intellij.testFramework.LightPlatformTestCase
import org.jetbrains.kotlin.idea.KotlinLanguage
import org.jetbrains.kotlin.psi.{KtFile, KtPsiFactory}
import org.jetbrains.plugins.scala.base.ScalaLightPlatformCodeInsightTestCaseAdapter
import org.jetbrains.plugins.scala.extensions.inWriteAction
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.junit.Assert._

abstract class ConverterTestBase extends ScalaLightPlatformCodeInsightTestCaseAdapter {

  private def formatKotlinCode(unformatedCode: String): String = {
    val ktPsiFactory = new KtPsiFactory(LightPlatformTestCase.getProject)
    val ktFile = ktPsiFactory.createFile(unformatedCode)
    Utils.reformatKtElement(ktFile)
    ktFile.getText.trim.split('\n').filterNot(_.isEmpty).mkString("\n")
  }

  def createKtFile(text: String): KtFile = {
    PsiFileFactory
      .getInstance(project)
      .createFileFromText("dummy.kt", KotlinLanguage.INSTANCE, text, true, false)
      .asInstanceOf[KtFile]
  }


  def doTest(scala: String, kotlin: String): Unit = {
    configureFromFileTextAdapter("dummy.scala", scala)
    val scalaFile = getFileAdapter.asInstanceOf[ScalaFile]

    val converter = new ScalaToKotlinLanguageConverter

    val convertedCode = inWriteCommand {
      val intermediateResult = converter.convertPsiElementToInternalRepresentation(scalaFile)
      val result = converter.convertInternalRepresentationToText(intermediateResult.getFirst,
        intermediateResult.getSecond, getProjectAdapter)
      val ktFile = createKtFile(result.getFirst)
      converter.runPostProcessOperations(ktFile, result.getSecond)
      ktFile.getText
    }

    val formatedExpected = formatKotlinCode(kotlin)
    val formatedActual = formatKotlinCode(convertedCode)
    assertEquals(formatedExpected, formatedActual)
  }

  private def inWriteCommand[T](data: => T): T = {
    var result: T = null.asInstanceOf[T]
    CommandProcessor
      .getInstance()
      .executeCommand(project, () => {
        CommandProcessor.getInstance().markCurrentCommandAsGlobal(project)
        result = inWriteAction(data)
      }, "", null)
    result
  }

  def doExprTest(scala: String, kotlin: String): Unit = {
    val (imports, expressionCode) =
      kotlin.split('\n').partition(_.startsWith("imports"))

    val newString =
      s"""${imports.mkString("\n")}
         |fun a(): Int {${expressionCode.mkString("\n")}
         |return 42 }""".stripMargin

    doTest(s"def a = {$scala \n 42}", formatKotlinCode(newString))
  }

}
