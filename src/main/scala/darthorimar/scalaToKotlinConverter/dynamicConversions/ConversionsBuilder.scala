package darthorimar.scalaToKotlinConverter.dynamicConversions

import com.intellij.openapi.project.Project
import darthorimar.scalaToKotlinConverter.ast.Defn
import darthorimar.scalaToKotlinConverter.step.ASTGenerationStep
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.project.ProjectContext

object ConversionsBuilder {
  def build(conversions: List[ConversionSource], project: Project): List[Conversion] =
    conversions.map(createConversion(_, project))


  def createConversion(conversionSource: ConversionSource, project: Project): Conversion = {
    val objectDefinition = new ASTGenerationStep().recover[Defn](buildDambObject(conversionSource, project))
    val scalaTemplate =
      objectDefinition.body.get.exprs.head.asInstanceOf[Defn].body.get.exprs.head
    val parameters = objectDefinition.body.get.exprs
    Conversion(parameters, scalaTemplate, conversionSource.kotlin)
  }


  private def buildDambObject(conversion: ConversionSource, project: Project) =
    ScalaPsiElementFactory.createScalaFileFromText(
      s"""object Dumb {
         |  ${conversion.parameters}
         |
         |  object Template {
         |    ${conversion.scala}
         |  }
         |}
      """.stripMargin)(new ProjectContext(project))

  private def handleParameters(scalaCode: String) = {
    val parameterRegex = raw"#{(\w*)}".r
    parameterRegex.replaceAllIn(scalaCode, m => m.group(1) + "__")

  }
}
