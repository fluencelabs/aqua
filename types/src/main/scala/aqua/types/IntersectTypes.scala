package aqua.types

import cats.Monoid
import cats.data.NonEmptyMap

/**
 * Intersection of types makes a new type that can be found in both arguments
 *
 * X := Intersection of A, B =>
 * X belongs to A
 * X belongs to B
 */
case class IntersectTypes(scalarsCombine: ScalarsCombine.T) extends Monoid[Type]:

  override def empty: Type = TopType

  def combineProducts(ap: ProductType, bp: ProductType): ProductType =
    ProductType(
      ap.toList.zip(bp.toList).map(combine)
    )

  override def combine(a: Type, b: Type): Type =
    (a, b) match {
      case _ if CompareTypes(a, b) == 0.0 => a

      case (ap: ProductType, bp: ProductType) =>
        combineProducts(ap, bp)

      case (as: StructType, bs: StructType) =>
        NonEmptyMap
          .fromMap(as.fields.toSortedMap.flatMap { case (ak, at) =>
            bs.fields.lookup(ak).map(bt => ak -> combine(at, bt))
          })
          .fold(empty)(fields => StructType(s"${as.name}_x_${bs.name}", fields))

      case (aa: ArrowType, bb: ArrowType) =>
        ArrowType(
          UniteTypes.top.combineProducts(aa.domain, bb.domain),
          combineProducts(aa.codomain, bb.codomain)
        )

      case (ac: OptionType, bc: BoxType) =>
        OptionType(ac.element `∩` bc.element)

      case (ac: BoxType, bc: OptionType) =>
        OptionType(ac.element `∩` bc.element)

      case (ac: ArrayType, bc: BoxType) =>
        ArrayType(ac.element `∩` bc.element)
      case (ac: BoxType, bc: ArrayType) =>
        ArrayType(ac.element `∩` bc.element)
      case (ac: StreamType, bc: StreamType) =>
        StreamType(ac.element `∩` bc.element)

      case (a: ScalarType, b: ScalarType) =>
        scalarsCombine(a, b)

      case _ =>
        CompareTypes.apply(a, b) match {
          case 1.0 => b
          case -1.0 => a
          case 0.0 => b
          case _ =>
            BottomType
        }
    }

object IntersectTypes:
  val top: IntersectTypes = IntersectTypes(ScalarsCombine.top)
  val bottom: IntersectTypes = IntersectTypes(ScalarsCombine.bottom)
