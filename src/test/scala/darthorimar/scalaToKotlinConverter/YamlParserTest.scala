package darthorimar.scalaToKotlinConverter

import darthorimar.scalaToKotlinConverter.dynamicConversions.YamlParser
import junit.framework.TestCase
import org.junit.Test

class YamlParserTest extends TestCase {
  def testYaml(): Unit =
    println(YamlParser.parse("""conversions:
                       |  - parameters: |
                       |        val x: Option[Int]
                       |        val f: Int => String
                       |    scala: |
                       |        x.map(f)
                       |    kotlin: |
                       |        x?.let { f(it) }""".stripMargin))
}
