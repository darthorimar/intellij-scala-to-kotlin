package darthorimar.scalaToKotlinConverter.dynamicConversions

import darthorimar.scalaToKotlinConverter.ast.{AST, RefExpr}
import org.meerkat.Syntax._
import org.meerkat.graph.parseGraphFromAllPositions
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers._
import org.meerkat.sppf.NonPackedNode

import scala.collection.mutable

object GrammarBuilder {
  def buildAndApplyGrammar(template: AST)(
    implicit input: TemplateMeerkatInput
  ): Stream[(Int, Option[(String, Int)])] = {
    def nodeParser(node: Any): Vertex[NodeType] = {
      def compare(real: Any, expected: Any): Boolean =
        real -> expected match {
          case (realAst: AST, nodeAst: AST) =>
            realAst.productPrefix == nodeAst.productPrefix
          case (Some(realValue), Some(nodeValue)) =>
            compare(realValue, nodeValue)
          case (realOne, nodeOne) => realOne.toString == nodeOne.toString
        }
      V((real: NodeType) => compare(real, node))
    }

    val noEdge = epsilonEdge ^ ((_: EdgeType) => None)
    val noVertex = epsilonVertex ^ ((_: NodeType) => None)

    def handlePair(pair: (String, Any)) =
      pair match {
        case (name, node) => outE(name) ~ build(node)
      }
    def buildAlternations(ast: AST) =
      ast.fields.toList match {
        case Nil => noEdge | noEdge//| noVertex
        case (firstName, firstAst) :: others =>
          (seqToAlt(outE(firstName) ~ build(firstAst)) /: others)(
            _ | handlePair(_)
          ) //| noEdge | noVertex
      }
    def build[Result](templatePart: Any): Parser & Option[(String, Int)] =
      templatePart match {
        case refExpr: RefExpr if refExpr.referenceName.endsWith(paramSuffix) =>
          syn[EdgeType, NodeType, Option[(String, Int)]](
            V[NodeType]((_: NodeType) => true) ^ (
              (_: NodeType) => refExpr.id.map(refExpr.referenceName -> _)//might be error-prone
            )
          )
        case astTemplatePart: AST =>
          syn(
            nodeParser(templatePart) ~ syn(buildAlternations(astTemplatePart))
          )
        case other => syn(nodeParser(other) ^ ((_: NodeType) => None))
      }

    val parser = syn( syn(nodeParser(template) ^ {
      case ast: AST => ast.id.get
      case _        => -1
    }) ~ syn(buildAlternations(template))) & {
      case sth => sth
    }
    val r = executeQuery(parser, input).toList
    Stream.empty
  }

}
