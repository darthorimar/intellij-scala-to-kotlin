package org.jetbrains.plugins.kotlinConverter.scopes

import com.intellij.openapi.util.TextRange
import org.jetbrains.plugins.kotlinConverter.ast._

case class ASTGeneratorState(precalculated: Map[TextRange, AST])