package darthorimar.scalaToKotlinConverter.dynamicConversions
import net.jcazevedo.moultingyaml._
import net.jcazevedo.moultingyaml.DefaultYamlProtocol._

object YamlParser {
  case class ParseResult(conversions: List[ConversionSource])
  implicit val conversionYaml = yamlFormat3(ConversionSource)
  implicit val resultYaml = yamlFormat1(ParseResult)

  def parse(content: String): List[ConversionSource] =
    content.parseYaml.convertTo[ParseResult].conversions
}
