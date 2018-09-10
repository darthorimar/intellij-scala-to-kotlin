package darthorimar.scalaToKotlinConverter.definition

case class FileDefinition(name: String,
                          override val dependencies: Seq[Definition] = Seq.empty,
                          private val containedDefinitions: Seq[String] = Seq.empty)
    extends Definition {
  def filename                     = s"$name.kt"
  def usedDefinitions: Seq[String] = name +: containedDefinitions
}
