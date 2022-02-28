package aqua.types

object ScalarsCombine {
  type T = (ScalarType, ScalarType) => Type

  def bottom(a: ScalarType, b: ScalarType): Type =
    CompareTypes(a, b) match {
      case 1.0 => b
      case -1.0 => a
      case 0.0 => b
      case _ =>
        (a, b) match {
          case (ScalarType.i64, ScalarType.u64) | (ScalarType.u64, ScalarType.i64) => ScalarType.u32
          case (ScalarType.i64 | ScalarType.i32, ScalarType.u64 | ScalarType.u32) |
              (ScalarType.u64 | ScalarType.u32, ScalarType.i64 | ScalarType.i32) =>
            ScalarType.u16
          case (
                ScalarType.i64 | ScalarType.i16 | ScalarType.i32,
                ScalarType.u64 | ScalarType.u16 | ScalarType.u32
              ) | (
                ScalarType.u64 | ScalarType.u16 | ScalarType.u32,
                ScalarType.i64 | ScalarType.i16 | ScalarType.i32
              ) =>
            ScalarType.u8
          case _ => BottomType
        }
    }

  def top(a: ScalarType, b: ScalarType): Type =
    CompareTypes(a, b) match {
      case 1.0 => a
      case -1.0 => b
      case 0.0 => a
      case _ =>
        (a, b) match {
          case (ScalarType.i8, ScalarType.u8) | (ScalarType.u8, ScalarType.i8) => ScalarType.i16
          case (ScalarType.i8 | ScalarType.i16, ScalarType.u8 | ScalarType.u16) |
              (ScalarType.u8 | ScalarType.u16, ScalarType.i8 | ScalarType.i16) =>
            ScalarType.i32
          case (
                ScalarType.i8 | ScalarType.i16 | ScalarType.i32,
                ScalarType.u8 | ScalarType.u16 | ScalarType.u32
              ) | (
                ScalarType.u8 | ScalarType.u16 | ScalarType.u32,
                ScalarType.i8 | ScalarType.i16 | ScalarType.i32
              ) =>
            ScalarType.i64
          case _ => TopType
        }
    }

}
