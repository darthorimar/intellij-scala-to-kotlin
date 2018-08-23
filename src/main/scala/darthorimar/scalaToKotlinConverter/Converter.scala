package darthorimar.scalaToKotlinConverter

import com.intellij.openapi.application.ApplicationManager
import com.intellij.psi.PsiElement
import darthorimar.scalaToKotlinConverter.ast._
import darthorimar.scalaToKotlinConverter.builder.KotlinBuilder
import darthorimar.scalaToKotlinConverter.pass.Transform
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.transformation.Transformer
import org.jetbrains.plugins.scala.lang.transformation.calls._

object Converter {
  private val transformers: Set[Transformer] = Set(
    new ExpandApplyCall(),
    new ImplicitTransform()
  )


  case class ConvertResult(files: Seq[(String, ScalaPsiElement, Collector)])

  def convert(psis: Seq[ScalaPsiElement], doPrint: Boolean = false): ConvertResult = {
    val convertedFiles = psis.map { psi =>
      ApplicationManager.getApplication.runWriteAction(new Runnable {
        override def run(): Unit =
          Transformer.transform(psi, None, transformers)
      })
      val builder: KotlinBuilder = new KotlinBuilder
      val astGenerator = new ASTGenerator
      val ast = astGenerator.gen[AST](psi)

      if (doPrint) println(Utils.prettyPrint(ast))

      val (newAst, collector) = Transform(ast, astGenerator)
      builder.gen(newAst)
      (builder.text, psi, collector)
    }
    ConvertResult(convertedFiles)
  }
}
