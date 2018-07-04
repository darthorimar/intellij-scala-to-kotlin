package org.jetbrains.plugins.kotlinConverter

trait KotlinBuilderBase {
  private val builder = new StringBuffer()
  private var i = 0
  private val indentStep = 2
  def str(v: Any): Unit =
    builder.append(v)

  def nl(): Unit = {
    str("\n")
    str(" " * i)
  }

  def indent(): Unit = {
    i += indentStep
    nl()
  }
  def unIndent(): Unit = {
    i -= indentStep
    nl()
  }

  def rep[T](values: Seq[T], sep: => Unit)(h: T => Unit): Unit =
    if (values.nonEmpty) {
      values.init.foreach { value =>
        h(value)
        sep
      }
      h(values.last)
    }

  def rep[T](values: Seq[T], sep: String)(h: T => Unit): Unit =
    rep(values, str(sep))(h)

  def repNl[T](values: Seq[T])(h: T => Unit): Unit =
    rep(values, nl)(h)

  def text: String = builder.toString
}
