package org.jetbrains.plugins.kotlinConverter

import com.intellij.psi.PsiFile
import com.intellij.psi.codeStyle.CodeStyleManager
import org.jetbrains.plugins.kotlinConverter.ast.Type

object Utils {
  def escapeName(name: String): String =
    s"`$name`"


  def prettyPrint(a: Any, indentSize: Int = 2, depth: Int = 0): String = {
    val indent = " " * depth * indentSize
    val fieldIndent = indent + (" " * indentSize)
    val thisDepth = prettyPrint(_: Any, indentSize, depth)
    val nextDepth = prettyPrint(_: Any, indentSize, depth + 1)
    a match {
      case t: Type =>
        t.asKotlin
      case Some(x) =>
        prettyPrint(x, indentSize, depth)
      case s: String =>
        val replaceMap = Seq(
          "\n" -> "\\n",
          "\r" -> "\\r",
          "\t" -> "\\t",
          "\"" -> "\\\""
        )
        '"' + replaceMap.foldLeft(s) { case (acc, (c, r)) => acc.replace(c, r) } + '"'
      case xs: Seq[_] if xs.isEmpty => xs.toString()
      case xs: Seq[_] =>
        val result = xs.map(x => s"\n$fieldIndent${nextDepth(x)}").toString()
        result.substring(0, result.length - 1) + "\n" + indent + ")"
      case p: Product =>
        val prefix = p.productPrefix
        val cls = p.getClass
        val fields = cls.getDeclaredFields.filterNot(_.isSynthetic).map(_.getName)
        val values = p.productIterator.toSeq
        if (fields.length != values.length) return p.toString
        fields.zip(values).toList match {
          case Nil => p.toString
          case (_, value) :: Nil => s"$prefix(${thisDepth(value)})"
          case kvps =>
            val prettyFields = kvps.map { case (k, v) => s"$fieldIndent$k = ${nextDepth(v)}" }
            s"$prefix(\n${prettyFields.mkString(",\n")}\n$indent)"
        }
      case _ => a.toString
    }
  }

  def reformatFile(file: PsiFile): Unit = {
    val manager = CodeStyleManager.getInstance(file.getProject)
    manager.reformatRange(file, 0, file.getTextLength)
  }
}
