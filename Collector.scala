package org.jetbrains.plugins.kotlinConverter

import org.jetbrains.plugins.kotlinConverter.builder.codegen.Definition

trait Collector {
  private var definitions: List[Definition] = Nil
  def addDefinition(definition: Definition): Unit =
    definitions = definition :: definitions

  def collectedDefinitions: Seq[Definition] =
    definitions
}
