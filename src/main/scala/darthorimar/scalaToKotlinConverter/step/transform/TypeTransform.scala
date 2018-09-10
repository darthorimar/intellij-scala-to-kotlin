package darthorimar.scalaToKotlinConverter.step.transform

import darthorimar.scalaToKotlinConverter
import darthorimar.scalaToKotlinConverter.types._
import darthorimar.scalaToKotlinConverter.ast._
import darthorimar.scalaToKotlinConverter.definition.{Definition, TupleDefinition}
import darthorimar.scalaToKotlinConverter.types.TypeUtils.ScalaTuple

class TypeTransform extends Transform {
  override def name: String = "Transforming types"

  override protected val action: PartialFunction[AST, AST] = {
    case FunctionType(ProductType(Seq(left)), right) =>
      FunctionType(transform[Type](left), transform[Type](right))

    case GenericType(inner, Seq(i)) if TypeUtils.isOption(transform[Type](inner)) =>
      NullableType(transform[Type](i))

    case StdTypes.ANY_REF | StdTypes.ANY =>
      StdTypes.ANY

    case ScalaTypes.STRING | ScalaTypes.JAVA_STRING =>
      StdTypes.STRING

    case ScalaTuple(2) =>
      KotlinTypes.PAIR

    case ScalaTuple(arity) =>
      stateStepVal.addDefinition(Definition.tuple(arity))
      LibTypes.tupleType(arity)

    case ScalaType("scala.util.Try") =>
      stateStepVal.addDefinition(Definition.tryDefinition)
      ClassType("Try")

    case ScalaType("scala.PartialFunction") =>
      stateStepVal.addDefinition(Definition.partialFunction)
      ClassType("PartialFunction")

    case ClassType(name) if name.stripPrefix("_root_.").startsWith("scala.") =>
      transform[Type](ScalaType(name))

    case ClassType(name) if name.stripPrefix("_root_.").startsWith("java.") =>
      transform[Type](JavaType(name))

    case JavaType("java.lang.Exception") =>
      KotlinType("Exception")

    case ScalaType("scala.collection.immutable.Nil$") =>
      GenericType(KotlinTypes.LIST, Seq(StdTypes.NOTHING))

    case ScalaTypes.SEQ |
         ScalaTypes.LIST |
         ScalaTypes.COLLECTION_IMMUTABLE_LIST |
         ScalaTypes.COLLECTION_LIST |
         ScalaTypes.COLLECTION_SEQ =>
      KotlinTypes.LIST

    case SimpleType(name) if name.startsWith("_root_.") =>
      SimpleType(name.stripPrefix("_root_."))

    case SimpleType("scala.collection.immutable.Nil.type") =>
      GenericType(KotlinTypes.LIST, Seq(NoType))
  }
}

