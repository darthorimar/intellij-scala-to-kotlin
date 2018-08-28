package darthorimar.scalaToKotlinConverter

import java.util

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.ServiceManager
import com.intellij.openapi.util.TextRange
import com.intellij.psi.{PsiDirectory, PsiDocumentManager, PsiElement, PsiFile}
import com.intellij.psi.codeStyle.CodeStyleManager
import darthorimar.scalaToKotlinConverter.ast.{Import, Type}
import org.jetbrains.kotlin.caches.resolve.KotlinCacheService
import org.jetbrains.kotlin.idea.j2k.J2kPostProcessor
import org.jetbrains.kotlin.idea.util.ImportInsertHelper
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.psi._
import org.jetbrains.kotlin.resolve.ImportPath
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.extensions.inWriteAction

object Utils {
  def createKotlinName(file: ScalaFile): String = {
    val nameWithoutExtension = file.getName.stripSuffix(".scala")
    nameWithoutExtension + ".kt"
  }


  def getSrcDir(psi: PsiElement): PsiDirectory = {
    def byPackageName(packageName: String): PsiDirectory = {
      if (packageName.nonEmpty) {
        val packageParts = packageName.split('.')
        val fileDirectory = psi.getContainingFile.getContainingDirectory
        val packagePath = packageParts.mkString("/")
        if (fileDirectory.getVirtualFile.getCanonicalPath.endsWith(packagePath)) {
          val goUp = (_: PsiDirectory).getParent
          val levelsUp = packageParts.length
          val getDirectory = Function.chain(Seq.fill(levelsUp)(goUp))
          getDirectory(fileDirectory)
        } else fileDirectory
      } else psi.getContainingFile.getContainingDirectory
    }

    psi match {
      case scalaFile: ScalaFile =>
        byPackageName(scalaFile.packageName)
      case ktFile: KtFile =>
        byPackageName(ktFile.getPackageFqName.asString)
      case _ =>
        getSrcDir(psi.getContainingFile)
    }
  }


  def escapeName(name: String): String =
    s"`$name`".replaceAllLiterally(":", "")


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

  def reformatKtElement(ktElement: KtElement): KtElement = inWriteAction {
    val manager = CodeStyleManager.getInstance(ktElement.getProject)
    manager.reformatRange(ktElement,
      ktElement.getTextRange.getStartOffset,
      ktElement.getTextRange.getEndOffset
    ).asInstanceOf[KtElement]
  }
}
