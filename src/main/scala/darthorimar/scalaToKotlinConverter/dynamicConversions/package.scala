package darthorimar.scalaToKotlinConverter

import darthorimar.scalaToKotlinConverter.ast.{AST, Expr, KotlinCodeExpr, Type}
import org.meerkat.parsers.Parsers.Nonterminal
import org.meerkat.parsers.&


import scala.language.implicitConversions

package object dynamicConversions {

  case class ConversionSource(parameters: String, scala: String, kotlin: String)

  case class Conversion(parameters: Seq[Expr],
                        scalaTemplate: Expr,
                        kotlinTemplate: String)

  val paramSuffix = "PARAM"

  type EdgeType = String
  type NodeType = Any
  type Data = (Int, Option[Int])
  type Parser = Nonterminal[EdgeType, NodeType]
  type DataParser = Parser & Data

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

    def id(implicit data: ASTInputData): Option[Int] =
      data.idByNode(ast)
  }

  def kotlinCodeExpr(expType: Type,
                     template: String,
                     holes: Map[String, AST]): KotlinCodeExpr = {
    val (codeParts, exprs) = splitParameters(template, holes)
    KotlinCodeExpr(expType, codeParts, exprs)
  }

  def splitParameters[A](code: String,
                         splitter: String => A): (Seq[String], Seq[A]) = {
    (paramRegex.split(code), paramRegex.findAllIn(code).map(splitter).toSeq)
  }

  def replaceParameters(code: String, replacer: String => String): String =
    paramRegex.replaceAllIn(code, g => replacer(g.group(1)))

  private val paramRegex = raw"\#\{(\w*)\}".r

}
