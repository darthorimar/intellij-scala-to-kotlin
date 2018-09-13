package darthorimar.scalaToKotlinConverter.step.transform

import com.intellij.openapi.project.Project
import darthorimar.scalaToKotlinConverter.ast._
import darthorimar.scalaToKotlinConverter.dynamicConversions.{Conversion, ConversionsBuilder, YamlParser}

class ApplyConversionsTransform(project: Project) extends Transform {
  override def name: String = "Applying internal transformations"

  val conversions = {
    val parsed =
      YamlParser.parse(
        s"""conversions:
           |  - parameters: |
           |        val x: Option[Int]
           |        val f: Int => String
           |    scala: |
           |        #{x}.map(#{f})
           |    kotlin: |
           |        #{x}?.let { #{f}(it) }""".stripMargin)
    ConversionsBuilder.build(parsed, project)
  }
  

  def tryApply(expr: Expr,conversion: Conversion) = conversion match {
    case Conversion(parameters, template, kotlinCode) =>
      def handle(exprPart: AST, templatePart: AST): Option[Seq[Expr]] =
        if (templatePart.productPrefix == exprPart.productPrefix) {
          templatePart match {
            case ref: RefExpr if ref.referenceName.endsWith("__") =>
              Some(Seq(exprPart.asInstanceOf))
            case _ =>
              (exprPart.productIterator zip templatePart.productIterator).map {
                case (real: AST, expected: AST) =>
                  handle(real, expected)
                case (realSeq: Seq[AST], expectedSeq: Seq[AST])
                  if realSeq.size == expectedSeq.size =>
                  (realSeq zip expectedSeq).map { case (r, e) => handle(r, e) }.sequence.map(_.flatten)
                case _ => None
              }.toSeq.sequence.map(_.flatten)             
              
          }
        } else None
      
      handle(expr, template) map (_ => KotlinCodeExpr(expr.exprType, kotlinCode))

  }

  implicit class TraversableSeqOfOptions[T](val seq: Seq[Option[T]]) {
    def sequence: Option[Seq[T]] =
      (Option(Seq.empty[T]) /: seq) {
        case (None, _) => None
        case (_, None) => None
        case (Some(xs),Some(x)) => Some(x +: xs)
      }
      
  }


  override protected val action: PartialFunction[AST, AST] = {
    case expr: Expr =>
      conversions.toStream map(tryApply(expr, _)) collectFirst {
        case Some(kotlinCode) => kotlinCode
      } getOrElse copy[Expr](expr)
    case ast => copy[AST](ast)
  }

}
