package aqua.context.walker

import aqua.parser.lexer._
import cats.Comonad
import cats.data.NonEmptyList
import cats.syntax.comonad._
import cats.syntax.functor._

case class Acc[T](data: Map[String, NonEmptyList[T]]) {

  def add(other: Acc[T], subtract: Set[String] = Set.empty): Acc[T] =
    copy(data = (other.data -- subtract).foldLeft(data) {
      case (accD, (k, v)) =>
        accD.updatedWith(k)(dv => Option(dv.fold(v)(_ ++ v.toList)))
    })

  def keys: Set[String] = data.keySet

  def sub(n: String): Acc[T] = copy(data = data - n)

  def erase: Acc[T] = Acc.empty[T]

  def addOne(n: String, v: T): Acc[T] = add(Acc.one(n, v))

  def takeKeys(keys: Set[String]): Acc[T] = copy(data = data.view.filterKeys(keys).toMap)
}

object Acc {
  def empty[T]: Acc[T] = Acc(Map.empty[String, NonEmptyList[T]])

  def one[T](n: String, v: T): Acc[T] = Acc(Map(n -> NonEmptyList.one(v)))

  def fromValues[F[_]: Comonad](args: List[Value[F]]): Acc[Value[F]] =
    args.collect {
      case arg @ VarLambda(name, _) => Acc.one[Value[F]](name.extract, arg)
    }.foldLeft(Acc.empty[Value[F]])(_ add _)

  def fromType[F[_]: Comonad](t: TypeToken[F]): Acc[CustomTypeToken[F]] =
    t match {
      case ct: CustomTypeToken[F] =>
        Acc.one(ct.name.extract, ct)
      case at: ArrayTypeToken[F] =>
        fromType(at.data)
      case at: ArrowTypeToken[F] =>
        at.args
          .widen[TypeToken[F]]
          .prependedAll(at.res)
          .map[Acc[CustomTypeToken[F]]](v => fromType[F](v))
          .foldLeft[Acc[CustomTypeToken[F]]](Acc.empty[CustomTypeToken[F]])(_ add _)
      case _: BasicTypeToken[F] =>
        Acc.empty
    }
}
