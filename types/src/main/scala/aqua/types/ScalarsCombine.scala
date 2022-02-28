package aqua.types
import cats.syntax.partialOrder.*

object ScalarsCombine {
  type T = (ScalarType, ScalarType) => Type

  def bottom(a: ScalarType, b: ScalarType): Type =
    CompareTypes(a, b) match {
      case 1.0 => b
      case -1.0 => a
      case 0.0 => b
      case _ =>
        ScalarType.all
          .filter((_: Type) <= a)
          .filter((_: Type) <= b)
          .filter(x => (ScalarType.float(a) || ScalarType.float(b)) || !ScalarType.float(x))
          .filter(x => (ScalarType.signed(a) || ScalarType.signed(b)) || !ScalarType.signed(x))
          .toList
          .sortWith((_: Type) > _)
          .headOption
          .getOrElse(BottomType)
    }

  def top(a: ScalarType, b: ScalarType): Type =
    CompareTypes(a, b) match {
      case 1.0 => a
      case -1.0 => b
      case 0.0 => a
      case _ =>
        ScalarType.all
          .filter((_: Type) >= a)
          .filter((_: Type) >= b)
          .filter(x => (ScalarType.float(a) || ScalarType.float(b)) || !ScalarType.float(x))
          .filter(x => (ScalarType.signed(a) || ScalarType.signed(b)) || !ScalarType.signed(x))
          .toList
          .sortWith((_: Type) < _)
          .headOption
          .getOrElse(TopType)
    }

}
