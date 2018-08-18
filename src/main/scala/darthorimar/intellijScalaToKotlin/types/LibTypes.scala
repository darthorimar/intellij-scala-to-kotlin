package darthorimar.intellijScalaToKotlin.types

import darthorimar.intellijScalaToKotlin.ast._

object LibTypes {
  def tupleType(arity: Int) = ClassType(s"Tuple$arity")
}
