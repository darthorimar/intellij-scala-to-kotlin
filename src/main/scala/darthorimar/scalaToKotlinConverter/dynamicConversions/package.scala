package darthorimar.scalaToKotlinConverter

import darthorimar.scalaToKotlinConverter.ast.{AST, Expr}
import org.meerkat.parsers.Parsers.Nonterminal

package object dynamicConversions {

  case class ConversionSource(parameters: String, scala: String, kotlin: String)

  case class Conversion(parameters: Seq[Expr],
                        scalaTemplate: Expr,
                        kotlinTemplate: String)

  val paramPrefix = "PARAM"

  type EdgeType = String
  type NodeType = AST
  type Parser = Nonterminal[EdgeType, NodeType]

  implicit class ASTOps(val ast: AST) extends AnyVal {
    private def fieldNames: Seq[String] =
      ast.getClass.getDeclaredFields.filterNot(_.isSynthetic) map { field =>
        s"${ast.productPrefix}#${field.getName}"
      }

    def fields: Seq[(String, AST)] =
      fieldNames zip ast.productIterator.toList flatMap {
        case (name, inner: AST)       => Seq(name -> inner)
        case (name, seq: Seq[AST])    => seq.map(name -> _)
        case (name, Some(value: AST)) => Seq(name -> value)
        case (name, simpleValue)      => Seq.empty //todo dont ignore in future
        //Seq(name -> SimpleValueWrapper(simpleValue))
      }
  }

  case class SimpleValueWrapper(value: Any) extends AST
}
