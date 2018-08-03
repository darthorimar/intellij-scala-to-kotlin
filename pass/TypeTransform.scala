package org.jetbrains.plugins.kotlinConverter.pass

import com.android.repository.impl.meta.TypeDetails.GenericType
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

    case GenerecTypes(SimpleType("_root_.scala.Tuple2"), Seq(p1, p2)) =>
      Some(GenerecTypes(KotlinTypes.PAIR, Seq(transform[Type](p1), transform[Type](p2))))


    case _ => None
  }
}

