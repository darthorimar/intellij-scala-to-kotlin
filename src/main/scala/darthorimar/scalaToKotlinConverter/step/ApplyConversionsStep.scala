package darthorimar.scalaToKotlinConverter.step

import com.intellij.openapi.project.Project
import darthorimar.scalaToKotlinConverter.ast._
import darthorimar.scalaToKotlinConverter.dynamicConversions.{
  Conversion,
  ConversionsBuilder,
  YamlParser,
  _
}
import darthorimar.scalaToKotlinConverter.step.ConverterStep.Notifier
import darthorimar.scalaToKotlinConverter.step.transform.Transform
import org.meerkat.graph.parseGraphFromAllPositions
import org.meerkat.sppf.{NonPackedNode, SPPFNode}

import scala.PartialFunction

class ApplyConversionsStep(project: Project) extends ConverterStep[AST, AST] {
  override def name: String = "Applying internal transformations"

  val conversions: List[Conversion] = {
    val parsed =
      YamlParser.parse(s"""
           |conversions:
           |  - parameters: |
           |        val x$paramSuffix: Option[Int]
           |        val f$paramSuffix: Int => String
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
    val conversion = conversions.head
    val (result, replacements) =
      GrammarBuilder.buildGrammarByTemplate(conversion.scalaTemplate, input)

    def collectNodes(node: SPPFNode): Seq[Int] =
      node match {
        case nonPackedNode: NonPackedNode =>
          Seq(nonPackedNode.leftExtent, nonPackedNode.rightExtent) ++
            node.children.flatMap(collectNodes)
        case _ => node.children.flatMap(collectNodes)
      }

    val kotlinCode =
      replaceParameters(conversion.kotlinTemplate, replacements map {
        case (k, v) => k.stripSuffix(paramSuffix) -> v
      })

    val possiblePoints =
      result
        .map(node => node.leftExtent -> node.rightExtent)
        .groupBy(_._1)
        .mapValues(_.map(_._2))
        .toSeq

    def getRealAccessable(origin: Int): Seq[Int] =
      input.data.nodesChildrenIds(origin) flatMap {
        case (_, id) => id +: getRealAccessable(id)
      }

    val pointOps = possiblePoints collectFirst {
      case (origin, children) if getRealAccessable(origin).toSet + origin == children.toSet + origin =>
        origin
    }

    val nodes = result.flatMap(node => Seq(node.leftExtent, node.rightExtent))

    input.print(nodes)
    println(result)
    pointOps map { point =>
      new Transform {
        override protected val action: PartialFunction[AST, AST] = {
          case ast if input.data.nodeById(point) contains ast =>
            KotlinCodeExpr(NoType, kotlinCode)
        }
        override def name: String = ""
      }.apply(from, state, 0, Notifier.empty)
    } getOrElse (from, state)
  }

}
