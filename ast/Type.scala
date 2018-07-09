package org.jetbrains.plugins.kotlinConverter.ast

trait Type extends AST {
  def asKotlin: String
}

case class FuncType(left: Type, right: Type) extends Type {
  override def asKotlin: String =
    s"(${left.asKotlin}) -> ${right.asKotlin}"
}

case class PType(des: Type, params: Seq[Type]) extends Type {
  override def asKotlin: String =
    params.map(_.asKotlin).mkString(des.asKotlin + "<", ", ", ">")
}

case class NullableType(t: Type) extends Type {
  override def asKotlin: String =
    t.asKotlin + "?"
}

case class ProdType(types: Seq[Type]) extends Type {
  override def asKotlin: String =
    types.map(_.asKotlin).mkString("(", ", ", ")")
}
case class SimpleType(name: String) extends Type {
  override def asKotlin: String = name
}

case object NoType extends Type {
  override def asKotlin: String = "Any"
}

object Types {
  val FUNCTION_PREF = "_root_.scala.Function"
  val OPTION = SimpleType("_root_.scala.Option")
}
