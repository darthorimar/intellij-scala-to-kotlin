package darthorimar.scalaToKotlinConverter.step.transform

import darthorimar.scalaToKotlinConverter
import darthorimar.scalaToKotlinConverter.types._
import darthorimar.scalaToKotlinConverter.ast._
import darthorimar.scalaToKotlinConverter.definition.{Definition, TupleDefinition}
import darthorimar.scalaToKotlinConverter.types.TypeUtils.ScalaTuple

class TypeTransform extends Transform {
  override protected def action(ast: AST): Option[AST] = ast match {
    case FunctionType(ProductType(Seq(left)), right) =>
      Some(FunctionType(transform[Type](left), transform[Type](right)))

    case GenericType(inner, Seq(i)) if TypeUtils.isOption(transform[Type](inner)) =>
      Some(NullableType(transform[Type](i)))

    case ScalaTypes.STRING | ScalaTypes.JAVA_STRING =>
      Some(StdTypes.STRING)

    case ScalaTuple(2) =>
      Some(KotlinTypes.PAIR)

    case ScalaTuple(arity) =>
      stateStepVal.addDefinition(Definition.tuple(arity))
      Some(LibTypes.tupleType(arity))

    case ScalaType("scala.util.Try") =>
      stateStepVal.addDefinition(Definition.tryDefinition)
      Some(ClassType("Try"))

    case ScalaType("scala.PartialFunction") =>
      stateStepVal.addDefinition(Definition.partialFunction)
      Some(ClassType("PartialFunction"))

    case ClassType(name) if name.stripPrefix("_root_.").startsWith("scala.") =>
      Some(transform[Type](ScalaType(name)))

    case ClassType(name) if name.stripPrefix("_root_.").startsWith("java.") =>
      Some(transform[Type](JavaType(name)))

    case JavaType("java.lang.Exception") =>
      Some(KotlinType("Exception"))

    case ScalaType("scala.collection.immutable.Nil$") =>
      Some(GenericType(KotlinTypes.LIST, Seq(StdTypes.NOTHING)))

    case ScalaTypes.SEQ |
         ScalaTypes.LIST |
         ScalaTypes.COLLECTION_IMMUTABLE_LIST |
         ScalaTypes.COLLECTION_LIST |
         ScalaTypes.COLLECTION_SEQ =>
      Some(KotlinTypes.LIST)

    case SimpleType(name) if name.startsWith("_root_.") =>
      Some(SimpleType(name.stripPrefix("_root_.")))

    case SimpleType("scala.collection.immutable.Nil.type") =>
      Some(GenericType(KotlinTypes.LIST, Seq(NoType)))

    case _ => None
  }
}

