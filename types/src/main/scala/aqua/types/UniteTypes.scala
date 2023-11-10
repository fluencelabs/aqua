package aqua.types

import aqua.errors.Errors.internalError

import cats.Monoid
import cats.data.NonEmptyMap
import scala.annotation.tailrec

/**
 * Union of types makes a new type that can take arguments of both types
 *
 * X := Union of A, B =>
 * A belongs to X
 * B belongs to X
 */
case class UniteTypes(scalarsCombine: ScalarsCombine.T) extends Monoid[Type]:

  override def empty: Type = BottomType

  def combineProducts(a: ProductType, b: ProductType): ProductType = {
    @tailrec def step(l: List[Type], r: List[Type], res: List[Type]): ProductType =
      (l, r) match {
        case (Nil, Nil) => ProductType(res)
        case (_ :: _, Nil) => ProductType(res.reverse ::: l)
        case (Nil, _ :: _) => ProductType(res.reverse ::: r)
        case (l :: lt, r :: rt) => step(lt, rt, IntersectTypes.bottom.combine(l, r) :: res)
      }

    step(a.toList, b.toList, Nil)
  }

  def combineDataTypes(a: DataType, b: DataType): DataType =
    (a `∪` b) match {
      case d: DataType => d
      case t => internalError(s"$a ∪ $b yields non-data type $t")
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
          IntersectTypes.bottom.combineProducts(aa.codomain, bb.codomain)
        )

      case (ac: OptionType, bc: ArrayType) =>
        ArrayType(combineDataTypes(ac.element, bc.element))
      case (ac: ArrayType, bc: OptionType) =>
        ArrayType(combineDataTypes(ac.element, bc.element))

      case (ac: ArrayType, bc: ArrayType) =>
        ArrayType(combineDataTypes(ac.element, bc.element))
      case (ac: OptionType, bc: OptionType) =>
        OptionType(combineDataTypes(ac.element, bc.element))

      case (ac: StreamType, bc: StreamType) =>
        StreamType(combineDataTypes(ac.element, bc.element))
      case (ac: StreamMapType, bc: StreamMapType) =>
        StreamMapType(combineDataTypes(ac.element, bc.element))

      case (a: ScalarType, b: ScalarType) =>
        scalarsCombine(a, b)

      case _ =>
        CompareTypes.apply(a, b) match {
          case 1.0 => a
          case -1.0 => b
          case 0.0 => a
          case _ => TopType
        }

    }

object UniteTypes:
  val top: UniteTypes = UniteTypes(ScalarsCombine.top)
  val bottom: UniteTypes = UniteTypes(ScalarsCombine.bottom)
