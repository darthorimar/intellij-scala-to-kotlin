package org.jetbrains.plugins.kotlinConverter.ast

case class Type(name: String) extends AST
case class TypeParam(ty: Type) extends AST
