package darthorimar.scalaToKotlinConverter.dynamicConversions
import darthorimar.scalaToKotlinConverter.ast.{
  AST,
  Expr,
  KotlinCodeExpr,
  NoType
}

import scala.collection.mutable
import scala.language.postfixOps

object ConversionsCollector {
  case class ConversionData(startPosition: Int, convertedTo: KotlinCodeExpr)

  def collectConversions(input: TemplateMeerkatInput,
                         conversions: Seq[Conversion]): Seq[ConversionData] =
    conversions flatMap {
      case Conversion(parameters, scalaTemplate, kotlinTemplate) =>
        val (result, replacements) =
          GrammarBuilder.buildAndApplyGrammar(scalaTemplate, input)

        val kotlinCode =
          replaceParameters(kotlinTemplate, replacements map {
            case (k, v) => k.stripSuffix(paramSuffix) -> v
          })

        val possiblePoints =
          result
            .map(node => node.leftExtent -> node.rightExtent)
            .groupBy(_._1)
            .mapValues(_.map(_._2))
            .toSeq
        import guru.nidi.graphviz.attribute.Color

        val colors =
          Seq(
            "#e6194b",
            "#3cb44b",
            "#ffe119",
            "#4363d8",
            "#f58231",
            "#911eb4",
            "#46f0f0",
            "#f032e6",
            "#bcf60c",
            "#fabebe",
            "#008080",
            "#e6beff",
            "#9a6324",
            "#fffac8",
            "#800000",
            "#aaffc3",
            "#808000",
            "#ffd8b1",
            "#000075",
            "#808080",
            "#ffffff",
            "#000000"
          ) map Color.rgb

//        input.print(
        val clrs = possiblePoints.zipWithIndex map {
          case ((from, tos), colorId) =>
            from -> (tos -> colorId)
        }

        val toColors = clrs flatMap {
          case (_, (to, color)) => to map (_ -> colors(color))
        } groupBy (_._1) mapValues (_.map(_._2)) mapValues {
          _ reduce (_ gradient _)
        } toSeq

        val fromColors = clrs map {
          case (from, (_, color)) => from -> colors(color).fill()
        }

        input.print(
          (toColors ++ fromColors) groupBy (_._1) mapValues (_.map(_._2))
        )

        def getRealAccessable(origin: Int): Seq[Int] =
          input.nodesChildrenIds(origin) flatMap {
            case (_, id) => id +: getRealAccessable(id)
          }

        possiblePoints collect {
          case (origin, children)
              if getRealAccessable(origin).toSet + origin == children.toSet + origin =>
            val exprTy = input.nodeById(origin) collect {
              case expr: Expr => expr.exprType
            } getOrElse NoType
            ConversionData(
              origin,
              KotlinCodeExpr(exprTy, Seq(kotlinCode), Seq.empty)
            )
        }
    }
}
