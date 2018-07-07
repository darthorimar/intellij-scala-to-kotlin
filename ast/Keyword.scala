package org.jetbrains.plugins.kotlinConverter.ast

trait Keyword extends AST

trait Attr extends Keyword

case object CaseAttr extends Attr
case object PublAttr extends Attr
case object PrivAttr extends Attr
case object ProtAttr extends Attr
case object OpenAttr extends Attr
case object FinalAttr extends Attr

trait ParamModifier extends Keyword
case object PrivModifier extends ParamModifier
case object PublModifier extends ParamModifier
case object NoModifier extends ParamModifier

trait ParamType extends Keyword
case object ValType extends ParamType
case object VarType extends ParamType
case object NoType extends ParamType

sealed trait DefnType extends Keyword
case object ClassDefn extends DefnType
case object TraitDefn extends DefnType
case object ObjDefn extends DefnType
