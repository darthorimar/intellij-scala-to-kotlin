package darthorimar.scalaToKotlinConverter.dynamicConversions

import darthorimar.scalaToKotlinConverter.ast.AST
import org.meerkat.input.Input

import scala.collection.mutable

class TemplateMeerkatInput(val root: AST) extends Input[EdgeType, NodeType] {
  val data = new ASTInputData(root)

  override def edgesCount: Int = data.nodesCount

  override def filterEdges(nodeId: Int,
                           predicate: EdgeType => Boolean,
                           outgoing: Boolean): Seq[(EdgeType, Int)] =
    data.nodesChildrenIds(nodeId) filter {
      case (name, _) if predicate(name) => true
      case _                            => false
    }

  override def checkNode(nodeId: Int,
                         predicate: NodeType => Boolean): Option[NodeType] =
    data.astById(nodeId) filter predicate

  def idToAst(id: Int): Option[AST] = data.astById(id)
}

class ASTInputData(val root: AST) {
  private val nodes = mutable.Map.empty[Int, mutable.ListBuffer[(String, Int)]]
  private val idToAst = mutable.Map.empty[Int, AST]

  private val queue = mutable.Queue[(Int, String, AST)]((0, "", root))
  private var index = 0
  while (queue.nonEmpty) {
    val (parentIndex, edgeLabel, element) = queue.dequeue()
    element match {
      case _: SimpleValueWrapper =>
      case ast: AST =>
        ast.fields foreach {
          case (name, a) => queue.enqueue((index, name, a))
        }
        idToAst(index) = ast
      case _ =>
    }
    nodes
      .getOrElseUpdate(parentIndex, new mutable.ListBuffer)
      .append((edgeLabel, index))
    index += 1
  }

  def nodesChildrenIds(parentId: Int): Seq[(String, Int)] =
    nodes(parentId)

  def nodesCount: Int = nodes.size

  def astById(id: Int): Option[AST] = idToAst.get(id)
}
