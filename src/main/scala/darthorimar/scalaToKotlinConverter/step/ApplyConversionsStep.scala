package darthorimar.scalaToKotlinConverter.step

import com.intellij.openapi.project.Project
import darthorimar.scalaToKotlinConverter.ast._
import darthorimar.scalaToKotlinConverter.dynamicConversions.{Conversion, ConversionsBuilder, YamlParser, _}
import org.meerkat.graph.parseGraphFromAllPositions

class ApplyConversionsStep(project: Project) extends ConverterStep[AST, AST] {
  override def name: String = "Applying internal transformations"

  val conversions = {
    val parsed =
      YamlParser.parse(s"""conversions:
           |  - parameters: |
           |        val x: Option[Int]
           |        val f: Int => String
           |    scala: |
           |        x.map(f)
           |    kotlin: |
           |        x.let { f(it) }""".stripMargin)
    ConversionsBuilder.build(parsed, project)
  }

  def tryApply(expr: Expr, conversion: Conversion) = conversion match {
    case Conversion(parameters, template, kotlinCode) =>
  }

  implicit class TraversableSeqOfOptions[T](val seq: Seq[Option[T]]) {
    def sequence: Option[Seq[T]] =
      (Option(Seq.empty[T]) /: seq) {
        case (None, _)           => None
        case (_, None)           => None
        case (Some(xs), Some(x)) => Some(x +: xs)
      }
  }
  override def apply(
    from: AST,
    state: ConverterStepState,
    index: Int,
    notifier: ConverterStep.Notifier
  ): (NodeType, ConverterStepState) = {
    notifier.notify(this, index)
    val input = new TemplateMeerkatInput(from)
    val conversion = conversions.head
    val parser = GrammarBuilder.buildGrammarByTemplate(conversion.scalaTemplate)
    val result = parseGraphFromAllPositions(parser, input)
    println(result)
    (from, state)
  }
}
