package darthorimar.scalaToKotlinConverter

import darthorimar.scalaToKotlinConverter.Converter.ConvertResult
//import org.jetbrains.plugins.scala.base.ScalaLightPlatformCodeInsightTestCaseAdapter
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.junit.Assert._

abstract class ConverterTestBase {// extends ScalaLightPlatformCodeInsightTestCaseAdapter {
  def doTest(scala: String,
             kotlin: String,
             doPrint: Boolean = false): Unit = {
//    configureFromFileTextAdapter("dummy.scala", scala)
//    val psiFile = getFileAdapter
//    val ConvertResult(files, definitions) = Converter.convert(Seq(psiFile.asInstanceOf[ScalaFile]), doPrint)
//    val res = files.head._1
//    if (doPrint) {
//      println(res)
//    }
//    else {
//      assertEquals(s"\n$res\n is not equals to\n $kotlin \n",
//        unformat(kotlin),
//        unformat(res))
//    }
  }


  def doExprTest(scala: String,
                 kotlin: String,
                 doPrint: Boolean = false): Unit =
    doTest(s"def a = {$scala \n 42}", s"fun a(): Int {$kotlin \n return 42}", doPrint)

  private def unformat(text: String) =
    text.replaceAllLiterally(" ", "").replaceAllLiterally("\n", "")
}
