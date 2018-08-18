package darthorimar.intellijScalaToKotlin.scopes

import com.intellij.openapi.util.TextRange
import darthorimar.intellijScalaToKotlin.ast._

case class ASTGeneratorState(precalculated: Map[TextRange, AST])