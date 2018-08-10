package org.jetbrains.plugins.kotlinConverter.definition

import org.jetbrains.plugins.kotlinConverter.builder.codegen.TupleDefinition

trait Definition {
  def name: String
  def dependencies: Seq[Definition] = Seq.empty
}

object Definition {
  val tryDefinition = FileDefinition("Try")
  val unzip3 = FileDefinition("unzip3")
  val partialFunction = FileDefinition("PartialFunction", Seq(matchError))
  val matchError = FileDefinition("MatchError")

  def tupleDefinition(arity: Int) = new TupleDefinition(arity)
}