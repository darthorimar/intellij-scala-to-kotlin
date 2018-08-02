package org.jetbrains.plugins.kotlinConverter.pass

import org.jetbrains.plugins.kotlinConverter.types._
import org.jetbrains.plugins.kotlinConverter.ast._

class TypeTransform extends Transform {
  override protected def action(ast: AST): Option[AST] = ast match {
    case FunctionType(ProductType(Seq(left)), right) =>
      Some(FunctionType(transform[Type](left), transform[Type](right)))

    case GenerecTypes(inner, Seq(i)) if TypeUtils.isOption(inner) =>
      Some(NullableType(transform[Type](i)))

    case ScalaTypes.STRING | ScalaTypes.JAVA_STRING =>
      Some(KotlinTypes.STRING)

    case ScalaTypes.SEQ | ScalaTypes.SEQ2 | ScalaTypes.LIST | ScalaTypes.LIST2 =>
      Some(KotlinTypes.LIST)


    case SimpleType(name) if name.startsWith("_root_.") =>
      Some(SimpleType(name.stripPrefix("_root_.")))

    case SimpleType("scala.collection.immutable.Nil.type") =>
      Some(GenerecTypes(KotlinTypes.LIST, Seq(NoType)))


    case _ => None
  }
}

