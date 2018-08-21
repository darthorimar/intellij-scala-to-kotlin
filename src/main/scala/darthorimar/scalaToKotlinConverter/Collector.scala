package darthorimar.scalaToKotlinConverter

import darthorimar.scalaToKotlinConverter.ast.Import
import darthorimar.scalaToKotlinConverter.definition.{Definition, DefinitionGenerator}


trait Collector {
  private var definitions: Set[Definition] = Set.empty
  private var imports: Set[Import] = Set.empty

  def addDefinition(definition: Definition): Unit = {
    imports += Import(DefinitionGenerator.packageName + ".*")
    definitions = definitions + definition
  }

  def addImport(imp: Import): Unit =
    imports = imports + imp

  def collectedDefinitions: Seq[Definition] =
    definitions.toSeq

  def collectImports: Seq[Import] =
    imports.toSeq
}