package darthorimar.scalaToKotlinConverter.step

import java.util

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.ServiceManager
import com.intellij.psi.PsiDocumentManager
import darthorimar.scalaToKotlinConverter.Utils
import darthorimar.scalaToKotlinConverter.ast.Import
import darthorimar.scalaToKotlinConverter.definition.{ Definition, DefinitionGenerator }
import darthorimar.scalaToKotlinConverter.step.ConverterStep.Notifier
import org.jetbrains.kotlin.caches.resolve.KotlinCacheService
import org.jetbrains.kotlin.idea.j2k.J2kPostProcessor
import org.jetbrains.kotlin.idea.util.ImportInsertHelper
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.psi.{ KtElement, KtFile, KtPsiFactory }
import org.jetbrains.kotlin.resolve.scopes.{ DescriptorKindFilter, MemberScope }
import org.jetbrains.kotlin.resolve.{ BindingTraceContext, ImportPath, QualifiedExpressionResolver }
import org.jetbrains.plugins.scala.extensions.inWriteAction

import collection.JavaConverters._

class GenerateKtElementStep extends ConverterStep[String, KtElement] {
  override def name: String = "Creating Kotlin File"

  override def apply(from: String,
                     state: ConverterStepState,
                     index: Int,
                     notifier: Notifier): (KtElement, ConverterStepState) = {

    notifier.notify(this, index)
    val ktElement = state.elementGenerator.get.insertCode(from)
    val file      = ktElement.getContainingFile
    generateDefinitions(state.collectedDefinitions, file.asInstanceOf[KtFile])
    generateImports(state.collectImports, file.asInstanceOf[KtFile])
    PsiDocumentManager
      .getInstance(ktElement.getProject)
      .commitDocument(PsiDocumentManager.getInstance(ktElement.getProject).getDocument(ktElement.getContainingFile))
    val formated = Utils.reformatKtElement(ktElement)
    (formated, state)
  }

  private def generateDefinitions(definitions: Seq[Definition], ktFile: KtFile): Unit = {
    DefinitionGenerator.generate(definitions, ktFile.getContainingDirectory)
  }

  private def generateImports(imports: Seq[Import], ktFile: KtFile): Unit = {
    ApplicationManager.getApplication.invokeAndWait { () =>
      inWriteAction {
        imports sortBy (_.ref) foreach (generateSingleImport(_, ktFile))
      }
    }
  }

  private def generateSingleImport(imp: Import, ktFile: KtFile): Unit =
    imp match {
      case Import(ref) =>
        val fqName = new FqName(ref)
        new J2kPostProcessor(false).insertImport(ktFile, fqName)
      //todo replace ??
      //        val project = ktFile.getProject
      //        val facade =
      //          ServiceManager.getService(project, classOf[KotlinCacheService])
      //            .getResolutionFacade(util.Collections.singletonList(ktFile))
      //
      //        val importDirective = new KtPsiFactory(project).createImportDirective(new ImportPath(fqName, importAll))
      //        val qualifiedExpressionResolver =
      //          facade.getFrontendService(facade.getModuleDescriptor, classOf[QualifiedExpressionResolver])
      //        val importReferences =
      //          Option(qualifiedExpressionResolver.processImportReference(importDirective,
      //            facade.getModuleDescriptor,
      //            new BindingTraceContext(),
      //            util.Collections.emptyList(), null)
      //          ) map (_.getContributedDescriptors(DescriptorKindFilter.ALL, _ => true, false).asScala
      //            ) getOrElse List.empty
      //
      //        importReferences.headOption foreach { ref =>
      //          ImportInsertHelper.getInstance(project).importDescriptor(ktFile, ref, false)
      //        }
    }
}
