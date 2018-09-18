package darthorimar.scalaToKotlinConverter.dynamicConversions

import com.intellij.openapi.project.Project
import darthorimar.scalaToKotlinConverter.ast.{BlockExpr, Defn, File, SimpleValOrVarDef}
import darthorimar.scalaToKotlinConverter.step.ASTGenerationStep
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.project.ProjectContext

object ConversionsBuilder {
  def build(conversions: List[ConversionSource], project: Project): List[Conversion] =
    conversions.map(createConversion(_, project))


  def createConversion(conversionSource: ConversionSource, project: Project): Conversion = {
    val objectDefinition =
      new ASTGenerationStep().recover[File](buildDambObject(conversionSource, project)).definitions.head.asInstanceOf[Defn].body.get.exprs
    val scalaTemplate =
      objectDefinition.last.asInstanceOf[Defn].body.get
        .exprs.head.asInstanceOf[SimpleValOrVarDef].expr
        .get.asInstanceOf[BlockExpr].exprs.head
    val parameters = objectDefinition.dropRight(1)
    Conversion(parameters, scalaTemplate, conversionSource.kotlin)
  }





  private def buildDambObject(conversion: ConversionSource, project: Project) =
    ScalaPsiElementFactory.createScalaFileFromText(
      s"""abstract class Dumb {
         |  ${handleParameters(conversion.parameters)}
         |
         |  object Template {
         |    val expr =  {
         |       ${handleParameters(conversion.scala)}
         |    }
         |  }
         |}
      """.stripMargin)(new ProjectContext(project))

  private def handleParameters(scalaCode: String) = {
    val parameterRegex = raw"\#\{(\w*)\}".r
    parameterRegex.replaceAllIn(scalaCode, m => m.group(1) + paramPrefix)

  }
}
