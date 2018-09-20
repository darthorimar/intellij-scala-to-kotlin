package darthorimar.scalaToKotlinConverter.dynamicConversions

import darthorimar.scalaToKotlinConverter.ast.AST
import org.meerkat.parsers.Parsers.Nonterminal
import org.meerkat.Syntax._
import org.meerkat.input.LinearInput._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._

object GrammarBuilder {
  def buildGrammarByTemplate(template: AST): Parser = {
    def nodeParser(node: Any): Vertex[NodeType] = {
      def compare(real: Any, expected: Any): Boolean =
        real -> expected match {
          case (realAst: AST, nodeAst: AST) =>
            realAst.productPrefix == nodeAst.productPrefix
          case (Some(realValue), Some(nodeValue)) =>
            compare(realValue, nodeValue)
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
              )
          }
        syn(nodeParser(templatePart) ~ syn(alternations), "root")
      case other => syn(nodeParser(other), "root")
    }
    build(template)
  }

}
