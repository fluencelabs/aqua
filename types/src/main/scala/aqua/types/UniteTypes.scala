package aqua.types

import cats.Monoid
import cats.data.NonEmptyMap

import scala.annotation.tailrec

object UniteTypes extends Monoid[Type]:

  override def empty: Type = BottomType

  def combineProducts(a: ProductType, b: ProductType): ProductType = {
    @tailrec def step(l: List[Type], r: List[Type], res: List[Type]): ProductType =
      (l, r) match {
        case (Nil, Nil) => ProductType(res)
        case (_ :: _, Nil) => ProductType(res.reverse ::: l)
        case (Nil, _ :: _) => ProductType(res.reverse ::: r)
        case (l :: lt, r :: rt) => step(lt, rt, IntersectTypes.combine(l, r) :: res)
      }

    step(a.toList, b.toList, Nil)
  }

  override def combine(a: Type, b: Type): Type =
    (a, b) match {
      case (ap: ProductType, bp: ProductType) =>
        combineProducts(ap, bp)

      case (as: StructType, bs: StructType) =>
        val asFields = as.fields.toSortedMap
        val bsFields = bs.fields.toSortedMap
        val sharedKeys = asFields.keySet intersect bsFields.keySet

        NonEmptyMap
          .fromMap(asFields.flatMap { case (ak, at) =>
            bs.fields.lookup(ak).map(bt => ak -> combine(at, bt))
          } ++ (asFields -- sharedKeys) ++ (bsFields -- sharedKeys))
          .fold(empty)(fields => StructType(s"${as.name}_U_${bs.name}", fields))

      case (aa: ArrowType, bb: ArrowType) =>
        // TODO test that both aa, bb are supertypes of this arrow
        ArrowType(
          combineProducts(aa.domain, bb.domain),
          IntersectTypes.combineProducts(aa.codomain, bb.codomain)
        )

      case (ac: OptionType, bc: ArrayType) =>
        ArrayType(ac.element `∪` bc.element)

      case (ac: ArrayType, bc: OptionType) =>
        ArrayType(ac.element `∪` bc.element)

      case (ac: ArrayType, bc: ArrayType) =>
        ArrayType(ac.element `∪` bc.element)
      case (ac: OptionType, bc: OptionType) =>
        OptionType(ac.element `∪` bc.element)

      case (ac: StreamType, bc: StreamType) =>
        StreamType(ac.element `∩` bc.element)

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

      case _ =>
        CompareTypes.apply(a, b) match {
          case 1.0 => a
          case -1.0 => b
          case 0.0 => a
          case _ => TopType
        }

    }
