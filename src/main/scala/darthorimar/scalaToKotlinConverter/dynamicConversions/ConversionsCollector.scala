package darthorimar.scalaToKotlinConverter.dynamicConversions
import darthorimar.scalaToKotlinConverter.ast.{
  AST,
  Expr,
  KotlinCodeExpr,
  NoType
}

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
