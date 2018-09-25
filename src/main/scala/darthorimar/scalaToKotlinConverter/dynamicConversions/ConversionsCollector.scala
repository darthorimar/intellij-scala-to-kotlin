package darthorimar.scalaToKotlinConverter.dynamicConversions
import darthorimar.scalaToKotlinConverter.ast.{Expr, NoType}
import guru.nidi.graphviz.attribute.Color

import scala.language.postfixOps

object ConversionsCollector {
  case class ConversionData(startPosition: Int,
                            convertedTemplate: String,
                            holes: Map[String, Int])

  private val visualisationColors: Seq[Color] =
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

  def collectConversions(
    conversions: Seq[Conversion]
  )(implicit input: TemplateMeerkatInput): Seq[ConversionData] =
    conversions flatMap {
      case Conversion(parameters, scalaTemplate, kotlinTemplate) =>
        val paths =
          GrammarBuilder.buildAndApplyGrammar(scalaTemplate)
        paths
      ???

//        val possiblePoints =
//          paths
//            .map()
//            .groupBy(_._1)
//            .mapValues(_.map(_._2))
//            .toSeq
//
//        def print(): Unit = {
//          val clrs = possiblePoints.zipWithIndex map {
//            case ((from, tos), colorId) =>
//              from -> (tos -> colorId)
//          }
//
//          val toColors = clrs flatMap {
//            case (_, (to, color)) => to map (_ -> visualisationColors(color))
//          } groupBy (_._1) mapValues (_.map(_._2)) mapValues {
//            _ reduce (_ gradient _)
//          } toSeq
//
//          val fromColors = clrs map {
//            case (from, (_, color)) => from -> visualisationColors(color).fill()
//          }
//
//          input.print(
//            (toColors ++ fromColors) groupBy (_._1) mapValues (_.map(_._2))
//          )
//        }
//        print()
//
//        def getRealAccessable(origin: Int): Seq[Int] =
//          input.nodesChildrenIds(origin) flatMap {
//            case (_, id) => id +: getRealAccessable(id)
//          }
//
//        possiblePoints collect {
//          case (origin, children)
//              if getRealAccessable(origin).toSet + origin == children.toSet + origin =>
//            ConversionData(origin, kotlinTemplate, holes)
//        }
    }
}
