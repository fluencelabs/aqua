package aqua.types

import cats.Monoid
import cats.data.NonEmptyMap

object UniteTypes extends Monoid[Type] :

  override def empty: Type = BottomType

  def combineProducts(a: ProductType, b: ProductType): ProductType =
    if (a.acceptsValueOf(b)) a
    else if (b.acceptsValueOf(a)) b
    else NilType

  override def combine(a: Type, b: Type): Type =
    CompareTypes.apply(a, b) match {
      case 1.0 => a
      case -1.0 => b
      case 0.0 => a
      case Double.NaN =>
        // Uncomparable types
        // But can we unite them?
        // And find such a type that inherits both a, b
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
              .fold(empty)(fields => StructType(s"${as.name} ∪ ${bs.name}", fields))

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

          case _ =>
            empty
        }

    }
