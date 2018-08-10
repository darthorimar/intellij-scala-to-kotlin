package org.jetbrains.plugins.kotlinConverter.ast

sealed trait Type extends AST {
  def asKotlin: String
  def isFunction: Boolean = false
}

case class FunctionType(left: Type, right: Type) extends Type {
  override def asKotlin: String = {
    val leftStr = left match {
      case t: ProductType => t.asKotlin
      case t => s"(${t.asKotlin})"
    }
    s"$leftStr -> ${right.asKotlin}"
  }

  override def isFunction: Boolean = true
}

case class GenericType(baseType: Type, parameters: Seq[Type]) extends Type {
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

case class ClassType(name: String) extends Type {
  override def asKotlin: String = name
}

case class StdType(name: String) extends Type {
  override def asKotlin: String = name
}

case class ScalaType(name: String) extends Type {
  override def asKotlin: String = name
}

case class JavaType(name: String) extends Type {
  override def asKotlin: String = name
}


case class KotlinType(name: String) extends Type {
  override def asKotlin: String = name
}

case class TypeParamType(typeParam: TypeParam) extends Type {
  override def asKotlin: String = typeParam.name
}

case object NoType extends Type {
  override def asKotlin: String = "Any"
}

case class ErrorType(text: String) extends Type with ErrorAst {
  override def asKotlin: String = ""
}