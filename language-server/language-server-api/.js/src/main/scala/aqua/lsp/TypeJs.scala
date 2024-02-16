package aqua.lsp

import aqua.types.*

import scala.scalajs.js.Dictionary
import scala.scalajs.js.JSConverters.*
import scalajs.js

sealed trait TypeJs extends js.Object {
  val tag: String
}

class ScalarTypeJs(val name: String) extends TypeJs {
  val tag: String = "scalar"
}

class ArrayTypeJs(val element: TypeJs) extends TypeJs {
  val tag: String = "array"
}

class OptionTypeJs(val element: TypeJs) extends TypeJs {
  val tag: String = "option"
}

class StreamTypeJs(val element: TypeJs) extends TypeJs {
  val tag: String = "stream"
}

class StreamMapTypeJs(val element: TypeJs) extends TypeJs {
  val tag: String = "streammap"
}

class CanonStreamTypeJs(val element: TypeJs) extends TypeJs {
  val tag: String = "canon"
}

class AbilityTypeJs(val name: String, val fields: js.Dictionary[TypeJs]) extends TypeJs {
  val tag: String = "ability"
}

class StructTypeJs(val name: String, val fields: js.Dictionary[TypeJs]) extends TypeJs {
  val tag: String = "struct"
}

class ServiceTypeJs(val name: String, val fields: js.Dictionary[TypeJs]) extends TypeJs {
  val tag: String = "service"
}

trait ProductType extends TypeJs

class LabeledConsTypeJs(val args: js.Dictionary[TypeJs]) extends TypeJs {
  val tag: String = "labeled"
}

class UnlabeledConsTypeJs(val types: js.Array[TypeJs]) extends TypeJs {
  val tag: String = "unlabeled"
}

class ArrowTypeJs(val domain: LabeledConsTypeJs, val codomain: UnlabeledConsTypeJs) extends TypeJs {
  val tag: String = "arrow"
}

class NilTypeJs extends TypeJs {
  val tag: String = "nil"
}

class BottomTypeJs extends TypeJs {
  val tag: String = "bottom"
}

class TopTypeJs extends TypeJs {
  val tag: String = "top"
}

object TypeJs {

  def typeList(types: Iterable[(String, Type)]): Dictionary[TypeJs] =
    js.Dictionary(types.map { case (n, t) =>
      (n, TypeJs.fromType(t))
    }: _*)

  def fromType(t: Type): TypeJs = {
    t match
      case ScalarType(name) => new ScalarTypeJs(name)
      case LiteralType(_, name) => new ScalarTypeJs(name)
      case ArrayType(el) => new ArrayTypeJs(fromType(el))
      case OptionType(el) => new OptionTypeJs(fromType(el))
      case StreamType(el) => new StreamTypeJs(fromType(el))
      case StreamMapType(el) => new StreamMapTypeJs(fromType(el))
      case CanonStreamType(el) => new CanonStreamTypeJs(fromType(el))
      case StructType(name, fields) => new StructTypeJs(name, typeList(fields.toSortedMap))
      case AbilityType(name, fields) => new AbilityTypeJs(name, typeList(fields.toSortedMap))
      case ServiceType(name, fields) => new ServiceTypeJs(name, typeList(fields.toSortedMap))
      case lct: LabeledConsType => new LabeledConsTypeJs(typeList(lct.toLabelledList()))
      case uct: UnlabeledConsType => new UnlabeledConsTypeJs(uct.toList.map(fromType).toJSArray)
      case ArrowType(domain, codomain) =>
        ArrowTypeJs(
          new LabeledConsTypeJs(typeList(domain.toLabelledList())),
          new UnlabeledConsTypeJs(codomain.toList.map(fromType).toJSArray)
        )
      case TopType => new TopTypeJs()
      case BottomType => new BottomTypeJs()
      case NilType => new NilTypeJs()
  }
}
