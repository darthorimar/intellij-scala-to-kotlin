package darthorimar.scalaToKotlinConverter

import java.io.OutputStreamWriter

import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFileFactory
import com.intellij.testFramework.LightPlatformTestCase
import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase
import darthorimar.scalaToKotlinConverter.step.{ConverterStepState, FileElementGenerator, KtElementGenerator}
import darthorimar.scalaToKotlinConverter.step.PrintKotlinCodeStep.KotlinCode
import kotlin.text.Charsets
import org.jetbrains.kotlin.idea.KotlinLanguage
import org.jetbrains.kotlin.psi.{KtElement, KtFile, KtPsiFactory}
import org.jetbrains.plugins.scala.base.ScalaLightPlatformCodeInsightTestCaseAdapter
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.extensions._
import org.junit.Assert._

abstract class ConverterTestBase extends ScalaLightPlatformCodeInsightTestCaseAdapter {

  private def formatKotlinCode(unformatedCode: String): String = {
    val ktPsiFactory = new KtPsiFactory(LightPlatformTestCase.getProject)
    val ktFile = ktPsiFactory.createFile(unformatedCode)
    Utils.reformatKtElement(ktFile)
    val formated = ktFile.getText.trim.split('\n').filterNot(_.isEmpty).mkString("\n")
    formated
  }

  class TestElementGenerator(project: Project) extends KtElementGenerator {
    override def insertCode(text: KotlinCode): KtElement = {
      PsiFileFactory.getInstance(project)
        .createFileFromText("dummy.kt", KotlinLanguage.INSTANCE, text, true, false).asInstanceOf[KtFile]
    }
  }

  def doTest(scala: String, kotlin: String): Unit = {
    configureFromFileTextAdapter("dummy.scala", scala)
    val scalaFile = getFileAdapter.asInstanceOf[ScalaFile]

//    val ktElementGenerator: KtElementGenerator =
//      (code: KotlinCode) => inWriteAction {
//        val ktVirtualFile = getSourceRootAdapter.createChildData(null, "dummy.kt")
//        val writer = new OutputStreamWriter(ktVirtualFile.getOutputStream(null), Charsets.UTF_8)
//        try writer.write(code)
//        finally writer.close()
//        getPsiManagerAdapter.findFile(ktVirtualFile).asInstanceOf[KtFile]
//      }


    val (ktFile: KtFile, _) =
      Converter.scalaPsiToKotlinPsi(scalaFile, new ConverterStepState(Some(new TestElementGenerator(getProjectAdapter))))
    val formatedExpected = formatKotlinCode(kotlin)
    val formatedActual = formatKotlinCode(ktFile.getText)
    assertEquals(formatedExpected, formatedActual)
  }

  def doExprTest(scala: String, kotlin: String): Unit = {
    val (imports, expressionCode) =
      kotlin.split('\n').partition(_.startsWith("imports"))

    val newKotlinCode =
      s"""${imports.mkString("\n")}
         |fun a(): Int {${expressionCode.mkString("\n")}
         |return 42 }""".stripMargin

    doTest(s"def a = {$scala \n 42}", formatKotlinCode(newKotlinCode))
  }

}
