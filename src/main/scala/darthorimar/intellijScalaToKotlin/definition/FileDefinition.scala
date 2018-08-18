package darthorimar.intellijScalaToKotlin.definition

case class FileDefinition(name: String, override val dependencies: Seq[Definition] = Seq.empty) extends Definition {
  def filename = s"$name.kt"
}
