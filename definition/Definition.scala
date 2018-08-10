package org.jetbrains.plugins.kotlinConverter.definition

import org.jetbrains.plugins.kotlinConverter.builder.codegen.TupleDefinition

trait Definition {
  def name: String
  def dependencies: Seq[Definition] = Seq.empty
}

object Definition {
  val tryDefinition = FileDefinition("Try", "Try.kt")
  def tupleDefinition(arity: Int) = new TupleDefinition(arity)
}