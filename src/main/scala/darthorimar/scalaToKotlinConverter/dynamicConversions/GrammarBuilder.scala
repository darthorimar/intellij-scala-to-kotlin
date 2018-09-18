package darthorimar.scalaToKotlinConverter.dynamicConversions

import darthorimar.scalaToKotlinConverter.ast.AST
import org.meerkat.parsers.Parsers.Nonterminal
import org.meerkat.Syntax._
import org.meerkat.input.LinearInput._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._

object GrammarBuilder {
  def buildGrammarByTemplate(template: AST): Parser = {
    def astNodeParser(ast: AST): Vertex[NodeType] =
      V((_: AST).productPrefix == ast.productPrefix)

    def build(templatePart: AST): Parser = {
      def handlePair(
        pair: (String, AST)
      ): SequenceBuilder[String, NodeType, NoValue] = pair match {
        case (name, ast) => outE(name) ~ build(ast)
      }
      val alternations =
        templatePart.fields.toList match {
          case Nil => parserToAlt(epsilon)
          case (firstName, firstAst) :: others =>
            (seqToAlt(outE(firstName) ~ build(firstAst)) /: others)(
              _ | handlePair(_)
            )
        }
      syn(astNodeParser(templatePart) ~ syn(alternations), "root")
    }
    build(template)
  }

}
