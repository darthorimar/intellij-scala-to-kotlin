package darthorimar.scalaToKotlinConverter.dynamicConversions

import java.io.File
import java.util

import darthorimar.scalaToKotlinConverter.ast.AST
import guru.nidi.graphviz.attribute.{Color, Label, Style}
import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.model.Factory.{graph, node, to}
import guru.nidi.graphviz.model.Node
import org.meerkat.input.Input

import scala.collection.mutable

class TemplateMeerkatInput(root: AST)
    extends ASTInputData(root)
    with Input[EdgeType, NodeType] {

  override def edgesCount: Int = nodesCount

  override def filterEdges(nodeId: Int,
                           predicate: EdgeType => Boolean,
                           outgoing: Boolean): Seq[(EdgeType, Int)] =
    nodesChildrenIds(nodeId) filter {
      case (name, _) if predicate(name) => true
      case _                            => false
    }

  override def checkNode(nodeId: Int,
                         predicate: NodeType => Boolean): Option[NodeType] =
    nodeById(nodeId) filter predicate

}

class ASTInputData(private val root: AST) {
  private val nodes = mutable.Map(-1 -> new mutable.ListBuffer[(String, Int)])
  private val idToNode = mutable.Map.empty[Int, Any]
  private val nodeToId = new util.IdentityHashMap[Any, Int]

  private val queue = mutable.Queue[(Int, String, Any)]((-1, "", root))
  private var index = 0
  while (queue.nonEmpty) {
    val (parentIndex, edgeLabel, element) = queue.dequeue()
    element match {
      case ast: AST =>
        ast.fields foreach {
          case (name, a) => queue.enqueue((index, name, a))
        }
      case _ =>
    }
    idToNode(index) = element
    nodeToId.put(element, index)
    nodes(index) = new mutable.ListBuffer
    nodes(parentIndex).append((edgeLabel, index))
    index += 1
  }

  def nodesChildrenIds(parentId: Int): Seq[(String, Int)] =
    nodes(parentId)

  def nodesCount: Int = nodes.size

  def nodeById(id: Int): Option[Any] = idToNode.get(id)

  def idByNode[T](node: T): Option[Int] =
    if (nodeToId.containsKey(node)) Some(nodeToId.get(node))
    else None

  def print(highlightNodes: Seq[Int]): Unit =
    new ASTToDotPrinter().print(root, "ast", this, highlightNodes)
}

class ASTToDotPrinter {
  val outputFolder = "outputGraphs"

  def print(root: AST,
            fileName: String,
            inputData: ASTInputData,
            highlightNodes: Seq[Int]): Unit = {

    def createNodeByName(name: String, id: Int): Node = {
      val newNode = node(s"$name#$id") `with` Label.of(s"($id) $name")
      if (highlightNodes contains id)
        newNode `with` (Color.LIGHTGREY, Style.FILLED)
      else newNode
    }

    def createNode(element: Any, id: Int): Node =
      element match {
        case ast: AST    => createNodeByName(ast.productPrefix, id)
        case Some(value) => createNode(value, id)
        case string: String =>
          createNodeByName('"' + string + '"', id) `with` Color.GREEN4.font()
        case boolean: Boolean =>
          createNodeByName(boolean.toString.capitalize, id) `with` Color.BLUE1
            .font()
        case None          => createNodeByName("None", id) `with` Color.BLUE1.font()
        case simpleElement => createNodeByName(simpleElement.toString, id)

      }

    def handleNode(currentNodeId: Int): Node = {
      val currentNode =
        createNode(inputData.nodeById(currentNodeId), currentNodeId)
      val childrenNodes = inputData.nodesChildrenIds(currentNodeId) map {
        case (edgeName, id) => edgeName -> handleNode(id)
      }
      (currentNode /: childrenNodes) {
        case (fromNode, (edgeName, toNode)) =>
          fromNode.link(to(toNode) `with` Label.of(edgeName))
      }
    }

    Graphviz
      .fromGraph(graph(fileName).directed `with` handleNode(0))
      .width(600)
      .render(Format.SVG_STANDALONE)
      .toFile(new File(s"$outputFolder/$fileName.svg"))
  }

}
