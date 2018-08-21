package org.jetbrains.plugins.kotlinConverter

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.extensions.{FirstChild, ImplicitConversion}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.transformation.{AbstractTransformer, bindTo, qualifiedNameOf}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.extensions.{FirstChild, ImplicitConversion}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaCode._
import org.jetbrains.plugins.scala.project.ProjectContext

import scala.util.Try

class ImplicitTransform extends AbstractTransformer {
  def transformation(implicit project: ProjectContext): PartialFunction[PsiElement, Unit] = {
    case e@ImplicitConversion(f: ScFunction)
      if !Try(f.getParent.getParent.getParent.asInstanceOf[ScObject].name).toOption.contains("Predef") =>
      val FirstChild(reference: ScReferenceExpression) = e.replace(code"${f.name}($e)")
      bindTo(reference, qualifiedNameOf(f))

    case e@ImplicitConversion(p: ScReferencePattern) =>
      val FirstChild(reference: ScReferenceExpression) = e.replace(code"${p.name}($e)")
      bindTo(reference, qualifiedNameOf(p))
  }

}
