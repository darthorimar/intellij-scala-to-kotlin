package darthorimar.scalaToKotlinConverter.definition

trait Definition {
  def name: String
  def dependencies: Seq[Definition] = Seq.empty
}

object Definition {
  val unzip3 = FileDefinition("unzip3")
  val matchError = FileDefinition("MatchError")
  val tryDefinition = FileDefinition("Try", Seq(matchError))
  val listCollect = FileDefinition("listCollect", Seq(matchError))
  val arrayCollect = FileDefinition("arrayCollect", Seq(matchError))
  val partialFunction = FileDefinition("PartialFunction", Seq(matchError))

  def tuple(arity: Int) = new TupleDefinition(arity)
}