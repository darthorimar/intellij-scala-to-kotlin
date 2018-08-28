package darthorimar.scalaToKotlinConverter.step

import darthorimar.scalaToKotlinConverter.ast.Import
import darthorimar.scalaToKotlinConverter.definition.{Definition, DefinitionGenerator}
import darthorimar.scalaToKotlinConverter.step.PrintKotlinCodeStep.KotlinCode
import org.jetbrains.kotlin.psi.KtElement

trait KtElementGenerator {
  def insertCode(text: KotlinCode): KtElement
}

class ConverterStepState {
  private var definitions: Set[Definition] = Set.empty
  private var imports: Set[Import] = Set.empty
  var elementGenerator: Option[KtElementGenerator] = None

  def addDefinition(definition: Definition): Unit = {
    imports += Import(DefinitionGenerator.packageName, importAll = true)
    definitions = definitions + definition
  }

  def addImport(imp: Import): Unit =
    imports = imports + imp

  def collectedDefinitions: Seq[Definition] =
    definitions.toSeq

  def collectImports: Seq[Import] =
    imports.toSeq
}