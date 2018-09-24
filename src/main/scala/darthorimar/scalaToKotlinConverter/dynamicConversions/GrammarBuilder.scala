package darthorimar.scalaToKotlinConverter.dynamicConversions

import darthorimar.scalaToKotlinConverter.ast.{AST, Expr}
import org.meerkat.parsers.Parsers.Nonterminal
import org.meerkat.Syntax._
import org.meerkat.graph.parseGraphFromAllPositions
import org.meerkat.input.LinearInput._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.sppf.NonPackedNode

import scala.collection.mutable

object GrammarBuilder {
  def buildAndApplyGrammar(
    template: AST,
    input: TemplateMeerkatInput
  ): (scala.Seq[NonPackedNode], mutable.Map[String, String]) = {
      val replacements = mutable.Map.empty[String, String]
    def nodeParser(node: Any): Vertex[NodeType] = {
      def compare(real: Any, expected: Any): Boolean =
        real -> expected match {
          case (realAst: AST, nodeAst: AST) =>
            realAst.productPrefix == nodeAst.productPrefix
          case (Some(realValue), Some(nodeValue)) =>
            compare(realValue, nodeValue)
          case (realString: String, expectedString: String)
              if expectedString.endsWith(paramSuffix) =>
            replacements(expectedString) = realString
            true
          case (realOne, nodeOne) => realOne.toString == nodeOne.toString
        }
      V((real: Any) => compare(real, node))
    }

    def build(templatePart: Any): Parser = templatePart match {
      case astTemplatePart: AST =>
        def handlePair(
          pair: (String, Any)
        ): SequenceBuilder[String, NodeType, NoValue] = pair match {
          case (name, node) => outE(name) ~ build(node)
        }
        val alternations =
          astTemplatePart.fields.toList match {
            case Nil => parserToAlt(epsilon)
            case (firstName, firstAst) :: others =>
              (seqToAlt(outE(firstName) ~ build(firstAst)) /: others)(
                _ | handlePair(_)
              ) | epsilon
          }
        syn(nodeParser(templatePart) ~ syn(alternations), "root")
      case other => syn(nodeParser(other), "root")
    }
    val parser = build(template)
    val result = parseGraphFromAllPositions(parser, input)
    result -> replacements
  }

}
