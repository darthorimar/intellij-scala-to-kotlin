package org.jetbrains.plugins.kotlinConverter.ast

trait AST

case class DefParam(ty: Type, name: String) extends AST
