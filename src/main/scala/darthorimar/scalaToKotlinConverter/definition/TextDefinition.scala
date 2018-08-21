package org.jetbrains.plugins.kotlinConverter.definition

import org.jetbrains.plugins.kotlinConverter.builder.BuilderBase

trait TextDefinition extends Definition with BuilderBase {
  protected def generate(): Unit
  def get: String = {
    generate()
    text
  }
}
