package darthorimar.scalaToKotlinConverter.step

import com.intellij.openapi.project.Project
import darthorimar.scalaToKotlinConverter.ast._
import darthorimar.scalaToKotlinConverter.dynamicConversions.ConversionsCollector.ConversionData
import darthorimar.scalaToKotlinConverter.dynamicConversions.{
  Conversion,
  ConversionsBuilder,
  YamlParser,
  _
}
import darthorimar.scalaToKotlinConverter.scopes.ScopedVal.scoped
import darthorimar.scalaToKotlinConverter.step.ConverterStep.Notifier
import darthorimar.scalaToKotlinConverter.step.transform.Transform
import org.meerkat.graph.parseGraphFromAllPositions
import org.meerkat.sppf.{NonPackedNode, SPPFNode}

import scala.PartialFunction
import scala.language.postfixOps

class ApplyConversionsStep(project: Project) extends ConverterStep[AST, AST] {
  override def name: String = "Applying internal transformations"

  val conversions: List[Conversion] = {
    val parsed =
      YamlParser.parse(s"""
           |conversions:
           |  - parameters: |
           |        val x$paramSuffix: Option[Int]
           |        val f$paramSuffix: Int => Int
           |    scala: |
           |        #{x}.map(#{f})
           |    kotlin: |
           |        #{x}.let { #{f}(it) }""".stripMargin)
    ConversionsBuilder.build(parsed, project)
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
  ): (AST, ConverterStepState) = {
    notifier.notify(this, index)
    val input = new TemplateMeerkatInput(from)

    val conversionsData =
      ConversionsCollector.collectConversions(input, conversions) map {
        case ConversionData(startPos, convertedTo) => startPos -> convertedTo
      } toMap

    new Transform {
      override protected val action: PartialFunction[AST, AST] =
        Function.unlift(input.idByNode(_).flatMap(conversionsData.get))
      override def name: String = ""
    }.apply(from, state, 0, Notifier.empty)
  }

}
