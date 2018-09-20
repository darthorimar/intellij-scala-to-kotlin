//package darthorimar.scalaToKotlinConverter
//import java.io.File
//
//import darthorimar.scalaToKotlinConverter.ast.{AST, Type}
//import darthorimar.scalaToKotlinConverter.scopes.LocalNamer
//import guru.nidi.graphviz.attribute.{Color, Label}
//import guru.nidi.graphviz.model.Factory._
//import guru.nidi.graphviz.model._
//import guru.nidi.graphviz.engine.Format
//import guru.nidi.graphviz.engine.Graphviz
//
//object ASTToDotPrinter {
//  val outputFolder = "outputGraphs"
//  val localNamer = new LocalNamer
//  def print(root: AST, name: String, withIds: Boolean): Unit = {
//    def createNode(element: Any) = {
//      val name = element match {
//        case ast: AST => ast.productPrefix
//        case other    => other.toString
//      }
//      node(localNamer.newName(name)).`with`(Label.of(name))
//    }
//    def handleNode(element: Any): Node =
//      element match {
//        case ty: Type =>
//          createNode(ty.asKotlin).`with`(Color.RED3.font())
//        case ast: AST =>
//          val fieldNames =
//            ast.getClass.getDeclaredFields
//              .filterNot(_.isSynthetic)
//              .map(_.getName)
//          val fieldsValues = ast.productIterator.toList
//          val currentNode = createNode(ast)
//
//          val childNodes = (fieldNames zip fieldsValues) map {
//            case (edgeName, element) => edgeName -> handleNode(element)
//          }
//          (currentNode /: childNodes) {
//            case (fromNode, (edgeName, toNode)) =>
//              fromNode.link(to(toNode).`with`(Label.of(edgeName)))
//          }
//
//        case seq: Seq[_] =>
//          val currentNode = createNode("list")
//          (currentNode /: seq.map(handleNode).zipWithIndex) {
//            case (fromNode, (toNode, index)) =>
//              fromNode.link(to(toNode).`with`(Label.of(index.toString), Color.LIGHTBLUE, Color.LIGHTBLUE.font()))
//          }
//
//        case Some(value) =>
//          handleNode(value)
//
//        case string: String =>
//          createNode('"' + string + '"').`with`(Color.GREEN4.font())
//
//        case boolean: Boolean =>
//          createNode(boolean.toString.capitalize).`with`(Color.BLUE1.font())
//
//        case None =>
//          createNode("None").`with`(Color.BLUE1.font())
//
//        case simpleElement =>
//          createNode(simpleElement.toString)
//      }
//
//    Graphviz
//      .fromGraph(graph(name).directed().`with`(handleNode(root)))
//      .width(600)
//      .render(Format.SVG_STANDALONE)
//      .toFile(new File(s"$outputFolder/$name.svg"))
//  }
//
//}
