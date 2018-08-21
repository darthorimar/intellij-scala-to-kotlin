  package org.jetbrains.plugins.kotlinConverter.ast

  sealed trait Keyword extends AST {
    def name: String
  }

  sealed trait Attribute extends Keyword

  case object NoAttribute extends Attribute {
    override def name: String = ""
  }

  case object CaseAttribute extends Attribute {
    override def name: String = "case"
  }
  case object DataAttribute extends Attribute {
    override def name: String = "data"
  }
  case object PublicAttribute extends Attribute {
    override def name: String = "public"
  }
  case object PrivateAttribute extends Attribute {
    override def name: String = "private"
  }
  case object ProtectedAttribute extends Attribute {
    override def name: String = "protected"
  }
  case object OpenAttribute extends Attribute {
    override def name: String = "open"
  }
  case object FinalAttribute extends Attribute {
    override def name: String = "final"
  }
  case object OverrideAttribute extends Attribute {
    override def name: String = "override"
  }
  case object AbstractAttribute extends Attribute {
    override def name: String = "abstract"
  }
  case object CompanionAttribute extends Attribute {
    override def name: String = "companion"
  }
  case object InternalAttribute extends Attribute {
    override def name: String = "internal"
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
