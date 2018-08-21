package darthorimar.scalaToKotlinConverter.definition

import darthorimar.scalaToKotlinConverter.builder.BuilderBase

trait TextDefinition extends Definition with BuilderBase {
  protected def generate(): Unit
  def get: String = {
    generate()
    text
  }
}
