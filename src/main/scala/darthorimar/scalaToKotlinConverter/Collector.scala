package darthorimar.scalaToKotlinConverter

import darthorimar.scalaToKotlinConverter.definition.{Definition, FileDefinition}

trait Collector {
  private var definitions: List[Definition] =  Nil
  def addDefinition(definition: Definition): Unit =
    definitions = definition :: definitions

  def collectedDefinitions: Seq[Definition] =
    definitions
}
