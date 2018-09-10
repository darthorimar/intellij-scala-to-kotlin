package darthorimar.scalaToKotlinConverter.definition

trait Definition {
  def name: String
  def dependencies: Seq[Definition] = Seq.empty
}

object Definition {
  val unzip3           = FileDefinition("unzip3")
  val matchError       = FileDefinition("MatchError")
  val tryDefinition    = FileDefinition("Try", Seq(matchError), Seq("runTry", "Failure", "Success"))
  val eitherDefinition = FileDefinition("Either", Seq.empty, Seq("Right", "Left"))
  val collect          = FileDefinition("collect", Seq(matchError))
  val partialFunction  = FileDefinition("PartialFunction", Seq(matchError))
  val stripSuffix      = FileDefinition("stripSuffix", Seq(matchError))

  def tuple(arity: Int) = new TupleDefinition(arity)
}
