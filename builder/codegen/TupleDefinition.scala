package org.jetbrains.plugins.kotlinConverter.builder.codegen


class TupleDefinition(arity: Int) extends Definition {
  def generate(): Unit = {
    str("data class Tuple")
    str(arity)
    str("<")
    rep(1 to arity, ", ") { i =>
      str("out T")
      str(i)
    }
    str(">(")
    rep(1 to arity, ", ") { i =>
      str("val _")
      str(i)
      str(": T")
      str(i)
    }
    str(")")
  }

  override def name: String = s"Tuple$arity"
}
