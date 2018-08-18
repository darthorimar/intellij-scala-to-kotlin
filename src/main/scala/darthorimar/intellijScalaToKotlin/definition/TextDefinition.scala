package darthorimar.intellijScalaToKotlin.definition

import darthorimar.intellijScalaToKotlin.builder.BuilderBase

trait TextDefinition extends Definition with BuilderBase {
  protected def generate(): Unit
  def get: String = {
    generate()
    text
  }
}
