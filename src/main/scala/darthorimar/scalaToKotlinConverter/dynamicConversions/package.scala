package darthorimar.scalaToKotlinConverter

import darthorimar.scalaToKotlinConverter.ast.{AST, Expr}
import org.meerkat.parsers.Parsers.Nonterminal

import scala.language.implicitConversions

package object dynamicConversions {

  case class ConversionSource(parameters: String, scala: String, kotlin: String)

  case class Conversion(parameters: Seq[Expr],
                        scalaTemplate: Expr,
                        kotlinTemplate: String)

  val paramSuffix = "PARAM"

  type EdgeType = String
  type NodeType = Any
  type Parser = Nonterminal[EdgeType, NodeType]

  implicit class ASTOps[T <: AST](val ast: T) extends AnyVal {
    private def fieldNames: Seq[String] =
      ast.getClass.getDeclaredFields.filterNot(_.isSynthetic) map { field =>
        s"${ast.productPrefix}#${field.getName}"
      }

    def fields: Seq[(String, Any)] =
      fieldNames zip ast.productIterator.toList flatMap {
        case (name, inner: AST) => Seq(name -> inner)
        case (name, seq: Seq[AST]) =>
          seq.zipWithIndex.map { case (a, id) => s"$name#$id" -> a }
        case (name, Some(value: AST)) => Seq(name -> value)
        case (name, simpleValue)      => Seq(name -> simpleValue)
      }

//    def withId: WithId[T] = {
//      def withIdRecursive[R](element: R): WithId[R] =
//        element match {
//          case ast: AST =>
//
//        }
//    }
  }
  def replaceParameters(code: String, replacer: String => String): String =
    raw"\#\{(\w*)\}".r.replaceAllIn(code, g => replacer(g.group(1)))

  case class WithId[T](id: Int, element: T)

  implicit def withIdToElement[T](withId: WithId[T]): T = withId.element
}
