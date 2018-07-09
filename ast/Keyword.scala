package org.jetbrains.plugins.kotlinConverter.ast

trait Keyword extends AST {
  def name: String
}

trait Attr extends Keyword

case object CaseAttr extends Attr {
  override def name: String = "data"
}
case object PublAttr extends Attr{
  override def name: String = "public"
}
case object PrivAttr extends Attr{
  override def name: String = "private"
}
case object ProtAttr extends Attr{
  override def name: String = "protected"
}
case object OpenAttr extends Attr{
  override def name: String = "open"
}
case object FinalAttr extends Attr{
  override def name: String = "final"
}

trait ParamModifier extends Keyword
case object PrivModifier extends ParamModifier{
  override def name: String = "private"
}
case object PublModifier extends ParamModifier{
  override def name: String = "public"
}
case object NoModifier extends ParamModifier{
  override def name: String = ""
}

trait ParamType extends Keyword
case object ValType extends ParamType {
  override def name: String = "val"
}
case object VarType extends ParamType {
  override def name: String = "var"
}
case object NoParamType extends ParamType {
  override def name: String = ""
}

sealed trait DefnType extends Keyword
case object ClassDefn extends DefnType {
  override def name: String = "class"
}
case object TraitDefn extends DefnType {
  override def name: String = "trait"
}
case object InterfaceDefn extends DefnType {
  override def name: String = "interface"
}
case object ObjDefn extends DefnType {
  override def name: String = "object"
}
