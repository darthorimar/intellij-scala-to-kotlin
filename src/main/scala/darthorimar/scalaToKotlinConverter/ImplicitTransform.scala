package darthorimar.scalaToKotlinConverter

import com.intellij.psi.PsiElement
import com.intellij.psi.impl.source.JavaDummyHolder
import org.jetbrains.plugins.scala.extensions.{FirstChild, ImplicitConversion}
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiElement
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaCode._
import org.jetbrains.plugins.scala.lang.transformation.{AbstractTransformer, bindTo, qualifiedNameOf}
import org.jetbrains.plugins.scala.project.ProjectContext


class ImplicitTransform extends AbstractTransformer {
  def transformation(implicit project: ProjectContext): PartialFunction[PsiElement, Unit] = {
    case e@ImplicitConversion(f: ScFunction) if !isDefaultFunction(f) =>
      val FirstChild(reference: ScReferenceExpression) = e.replace(code"${f.name}($e)")
      bindTo(reference, qualifiedNameOf(f))

    case e@ImplicitConversion(p: ScReferencePattern) if !isDefaultFunction(p) =>
      val FirstChild(reference: ScReferenceExpression) = e.replace(code"${p.name}($e)")
      bindTo(reference, qualifiedNameOf(p))
  }

  def isDefaultFunction(element: ScalaPsiElement): Boolean = element.getContainingFile match {
    case _: JavaDummyHolder => true
    case f: ScalaFile =>
      f.packageName == "scala" || f.getName == "Predef.scala"
    case _ => false
  }

}
