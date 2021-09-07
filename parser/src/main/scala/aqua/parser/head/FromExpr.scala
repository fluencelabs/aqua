package aqua.parser.head

import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Ability, Name}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.data.NonEmptyList
import cats.parse.Parser as P
import cats.~>

trait FromExpr[F[_]] {
  def imports: NonEmptyList[FromExpr.NameOrAbAs[F]]
}

object FromExpr {

  def mapK[F[_], K[_]: Comonad](imports: NonEmptyList[FromExpr.NameOrAbAs[F]])(fk: F ~> K): NonEmptyList[FromExpr.NameOrAbAs[K]] =
    imports.map {
      case Left((n, nOp)) => Left((n.mapK(fk), nOp.map(_.mapK(fk))))
      case Right(a, aOp) => Right((a.mapK(fk), aOp.map(_.mapK(fk))))
    }

  type NameOrAbAs[F[_]] = Either[Name.As[F], Ability.As[F]]

  def nameOrAbAs[F[_]: LiftParser: Comonad]: P[NameOrAbAs[F]] =
    Name.nameAs[F].map(Left(_)) | Ability.abAs[F].map(Right(_))

  def importFrom[F[_]: LiftParser: Comonad]: P[NonEmptyList[NameOrAbAs[F]]] =
      comma[NameOrAbAs[F]](nameOrAbAs[F]) <* ` ` <* `from`

  def show[F[_]](ne: NonEmptyList[NameOrAbAs[F]]): String =
    ne.toList.map(_.fold(
      non => non._1.value + non._2.map(_.value).fold("")(" as "+_),
      non => non._1.value + non._2.map(_.value).fold("")(" as "+_)
    )).mkString(", ")
}