package darthorimar.scalaToKotlinConverter.step.transform

import darthorimar.scalaToKotlinConverter
import darthorimar.scalaToKotlinConverter.types._
import darthorimar.scalaToKotlinConverter.ast._
import darthorimar.scalaToKotlinConverter.definition.{Definition, TupleDefinition}
import darthorimar.scalaToKotlinConverter.step.{ConverterStep, ConverterStepState}
import darthorimar.scalaToKotlinConverter.types.TypeUtils.{OptionType, ScalaTuple}

class TypeTransform extends Transform {
  override def name: String = "Transforming types"

  override def apply(from: AST, state: ConverterStepState, index: Int, notifier: ConverterStep.Notifier): (AST, ConverterStepState) = {
    val r = super.apply(from, state, index, notifier)
    r
  }

  override protected val action: PartialFunction[AST, AST] = {
    case FunctionType(ProductType(Seq(left)), right) =>
      FunctionType(transform[Type](left), transform[Type](right))

    case OptionType(inner) =>
      NullableType(transform[Type](inner))

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

