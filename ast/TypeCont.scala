package org.jetbrains.plugins.kotlinConverter.ast

case class TypeCont(real: Option[Type], inferenced: Option[Type]) extends AST {
  def realOfInf: Option[Type] =
    real.orElse(inferenced)
}

trait Type extends AST {
  def asKotlin: String
}

case class FuncType(left: Type, right: Type) extends Type {
  override def asKotlin: String =
    left.asKotlin + " => " + right.asKotlin
}
case class ProdType(types: Seq[Type]) extends Type {
  override def asKotlin: String =
    types.mkString("(", ", ", ")")
}
case class SimpleType(name: String) extends Type {
  override def asKotlin: String = name
}

case class TypeParam(ty: String) extends AST
