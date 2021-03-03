package aqua.context.walker

import aqua.parser.lexer._
import cats.Comonad
import cats.data.NonEmptyList
import cats.syntax.comonad._
import cats.syntax.functor._

case class Acc[F[_], T](data: Map[String, NonEmptyList[T]]) {

  def add(other: Acc[F, T], subtract: Set[String] = Set.empty): Acc[F, T] =
    copy(data = (other.data -- subtract).foldLeft(data) {
      case (accD, (k, v)) =>
        accD.updatedWith(k)(dv => Option(dv.fold(v)(_ ++ v.toList)))
    })

  def keys: Set[String] = data.keySet

  def sub(n: String): Acc[F, T] = copy(data = data - n)

  def erase: Acc[F, T] = Acc.empty[F, T]

  def addOne(n: String, v: T): Acc[F, T] = add(Acc.one(n, v))

  def takeKeys(keys: Set[String]): Acc[F, T] = copy(data = data.view.filterKeys(keys).toMap)
}

object Acc {
  def empty[F[_], T]: Acc[F, T] = Acc(Map.empty[String, NonEmptyList[T]])

  def one[F[_], T](n: String, v: T): Acc[F, T] = Acc(Map(n -> NonEmptyList.one(v)))

  def fromValues[F[_]: Comonad](args: List[Value[F]]): Acc[F, Value[F]] =
    args.collect {
      case arg @ VarLambda(name, _) => Acc.one[F, Value[F]](name.extract, arg)
    }.foldLeft(Acc.empty[F, Value[F]])(_ add _)

  def fromType[F[_]: Comonad](t: Type[F]): Acc[F, CustomType[F]] =
    t match {
      case ct: CustomType[F] =>
        Acc.one(ct.name.extract, ct)
      case at: ArrayType[F] =>
        fromType(at.data)
      case at: ArrowType[F] =>
        (at.res :: at.args.widen[Type[F]])
          .map[Acc[F, CustomType[F]]](v => fromType[F](v))
          .foldLeft[Acc[F, CustomType[F]]](Acc.empty[F, CustomType[F]])(_ add _)
      case _: BasicType[F] =>
        Acc.empty
    }
}
