package darthorimar.scalaToKotlinConverter.scopes

import com.intellij.openapi.util.TextRange
import darthorimar.scalaToKotlinConverter.ast._

case class ASTGeneratorState(precalculated: Map[TextRange, AST])
