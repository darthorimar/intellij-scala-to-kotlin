package org.jetbrains.plugins.kotlinConverter.definition

import org.jetbrains.plugins.kotlinConverter.builder.codegen.TupleDefinition

trait Definition {
  def name: String
  def dependencies: Seq[Definition] = Seq.empty
}

object Definition {
  val tryDefinition = FileDefinition("Try")
  val unzip3 = FileDefinition("unzip3")
  val matchError = FileDefinition("MatchError")
  val collect = FileDefinition("collect", Seq(matchError))
  val partialFunction = FileDefinition("PartialFunction", Seq(matchError))

  def tuple(arity: Int) = new TupleDefinition(arity)
}