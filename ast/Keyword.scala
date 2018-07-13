  package org.jetbrains.plugins.kotlinConverter.ast

  sealed trait Keyword extends AST {
    def name: String
  }

  sealed trait Attr extends Keyword

  case object NoAttr extends Attr {
    override def name: String = ""
  }

  case object CaseAttr extends Attr {
    override def name: String = "case"
  }
  case object DataAttr extends Attr {
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
  case object OverrideAttr extends Attr{
    override def name: String = "override"
  }


  sealed trait MemberKind extends Keyword
  case object ValKind extends MemberKind {
    override def name: String = "val"
  }
  case object VarKind extends MemberKind {
    override def name: String = "var"
  }
  case object NoMemberKind extends MemberKind {
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
