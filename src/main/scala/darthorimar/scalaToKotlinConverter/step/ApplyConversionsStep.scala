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
import scala.collection.mutable
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
    implicit val input: TemplateMeerkatInput = new TemplateMeerkatInput(from)

    val conversionsData =
      ConversionsCollector.collectConversions(conversions) map {
        case conversionData @ ConversionData(startPos, _, _) =>
          startPos -> conversionData
      } toMap

    new Transform {
      val newIds: mutable.Map[Int, AST] = mutable.Map.empty

      override def transform[T <: AST](ast: AST): T = {
        val newAst = super.transform[T](ast)
        newIds(ast.id.get) = newAst
        newAst
      }
      override protected val action: PartialFunction[AST, AST] = {
        case expr: Expr if expr.id.exists(conversionsData.contains) =>
          val conversionData = conversionsData(expr.id.get)
          kotlinCodeExpr(
            expr.exprType,
            conversionData.convertedTemplate,
            conversionData.holes.mapValues(newIds).toMap
          )

      }
      override def name: String = ""
    }.apply(from, state, 0, Notifier.empty)
  }

}
