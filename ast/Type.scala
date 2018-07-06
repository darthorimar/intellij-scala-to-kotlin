package org.jetbrains.plugins.kotlinConverter.ast

case class Type(real: Option[String], inferenced: Option[String]) extends AST {
  def realOfInf =
    real.orElse(inferenced)
}

case class TypeParam(ty: String) extends AST
