package org.jetbrains.plugins.kotlinConverter.ast

sealed trait Type extends AST {
  def asKotlin: String
  def isFunction: Boolean = false
}

case class FunctionType(left: Type, right: Type) extends Type {
  override def asKotlin: String =
    s"${left.asKotlin} -> ${right.asKotlin}"

  override def isFunction: Boolean = true
}

case class GenerecTypes(baseType: Type, parameters: Seq[Type]) extends Type {
  override def asKotlin: String =
    parameters.map(_.asKotlin).mkString(baseType.asKotlin + "<", ", ", ">")
}

case class NullableType(inner: Type) extends Type {
  override def asKotlin: String =
    inner.asKotlin + "?"
}

case class ProductType(types: Seq[Type]) extends Type {
  override def asKotlin: String =
    types.map(_.asKotlin).mkString("(", ", ", ")")
}
case class SimpleType(name: String) extends Type {
  override def asKotlin: String = name
}

case object NoType extends Type {
  override def asKotlin: String = "Any"
}

case object ErrorType extends Type with ErrorAst {
  override def asKotlin: String = ""
}