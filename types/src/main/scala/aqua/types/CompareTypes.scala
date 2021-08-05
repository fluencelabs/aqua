package aqua.types

import cats.data.NonEmptyMap

object CompareTypes {
  import Double.NaN

  private def cmpTypesList(l: List[Type], r: List[Type]): Double =
    if (l.length != r.length) NaN
    else if (l == r) 0.0
    else
      (l zip r).map(lr => apply(lr._1, lr._2)).fold(0.0) {
        case (a, b) if a == b => a
        case (`NaN`, _) => NaN
        case (_, `NaN`) => NaN
        case (0, b) => b
        case (a, 0) => a
        case _ => NaN
      }

  private def cmpStruct(lf: NonEmptyMap[String, Type], rf: NonEmptyMap[String, Type]): Double =
    if (lf.toSortedMap == rf.toSortedMap) 0.0
    else if (
      lf.keys.forall(rf.contains) && cmpTypesList(
        lf.toSortedMap.toList.map(_._2),
        rf.toSortedMap.view.filterKeys(lf.keys.contains).toList.map(_._2)
      ) == -1.0
    ) 1.0
    else if (
      rf.keys.forall(lf.contains) && cmpTypesList(
        lf.toSortedMap.view.filterKeys(rf.keys.contains).toList.map(_._2),
        rf.toSortedMap.toList.map(_._2)
      ) == 1.0
    ) -1.0
    else NaN

  private def cmpProd(l: ProductType, r: ProductType): Double = (l, r) match {
    case (NilType, NilType) => 0.0
    case (_: ConsType, NilType) => -1.0
    case (NilType, _: ConsType) => 1.0
    case (ConsType(lhead, ltail), ConsType(rhead, rtail)) =>
      // If any is not Cons, than it's Bottom and already handled
      val headCmp = apply(lhead, rhead)
      if (headCmp.isNaN) NaN
      else {
        val tailCmp = cmpProd(ltail, rtail)
        // If one is >, and another eq, it's >, and vice versa
        if (headCmp >= 0 && tailCmp >= 0) 1.0
        else if (headCmp <= 0 && tailCmp <= 0) -1.0
        else NaN
      }
  }

  /**
   * Compare types in the meaning of type variance.
   *
   * @param l Type
   * @param r Type
   * @return 0 if types match,
   *         1 if left type is supertype for the right one,
   *         -1 if left is a subtype of the right
   */
  def apply(l: Type, r: Type): Double =
    if (l == r) 0.0
    else
      (l, r) match {
        case (TopType, _) | (_, BottomType) => 1.0
        case (BottomType, _) | (_, TopType) => -1.0

        // Literals and scalars
        case (x: ScalarType, y: ScalarType) => ScalarType.scalarOrder.partialCompare(x, y)
        case (LiteralType(xs, _), y: ScalarType) if xs == Set(y) => 0.0
        case (LiteralType(xs, _), y: ScalarType) if xs(y) => -1.0
        case (x: ScalarType, LiteralType(ys, _)) if ys == Set(x) => 0.0
        case (x: ScalarType, LiteralType(ys, _)) if ys(x) => 1.0

        // Collections
        case (x: ArrayType, y: ArrayType) => apply(x.element, y.element)
        case (x: ArrayType, y: StreamType) => apply(x.element, y.element)
        case (x: ArrayType, y: OptionType) => apply(x.element, y.element)
        case (x: OptionType, y: StreamType) => apply(x.element, y.element)
        case (x: OptionType, y: ArrayType) => apply(x.element, y.element)
        case (x: StreamType, y: StreamType) => apply(x.element, y.element)
        case (StructType(_, xFields), StructType(_, yFields)) =>
          cmpStruct(xFields, yFields)

        // Products
        case (l: ProductType, r: ProductType) => cmpProd(l, r)

        // Arrows
        case (ArrowType(ldom, lcodom), ArrowType(rdom, rcodom)) =>
          val cmpDom = apply(ldom, rdom)
          val cmpCodom = apply(lcodom, rcodom)

          if (cmpDom >= 0 && cmpCodom <= 0) -1.0
          else if (cmpDom <= 0 && cmpCodom >= 0) 1.0
          else NaN

        case _ =>
          Double.NaN
      }
}
