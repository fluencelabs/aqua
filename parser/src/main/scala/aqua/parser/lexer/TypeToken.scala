package aqua.parser.lexer

import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import aqua.types.ScalarType
import cats.Comonad
import cats.parse.{Parser => P}
import cats.syntax.comonad._
import cats.syntax.functor._

sealed trait TypeToken[F[_]] extends Token[F]
sealed trait DataTypeToken[F[_]] extends TypeToken[F]

case class TopBottomToken[F[_]: Comonad](override val unit: F[Unit], isTop: Boolean)
    extends DataTypeToken[F] {
  override def as[T](v: T): F[T] = unit.as(v)
  def isBottom: Boolean = !isTop
}

case class ArrayTypeToken[F[_]: Comonad](override val unit: F[Unit], data: DataTypeToken[F])
    extends DataTypeToken[F] {
  override def as[T](v: T): F[T] = unit.as(v)
}

case class StreamTypeToken[F[_]: Comonad](override val unit: F[Unit], data: DataTypeToken[F])
    extends DataTypeToken[F] {
  override def as[T](v: T): F[T] = unit.as(v)
}

object StreamTypeToken {

  def `streamtypedef`[F[_]: LiftParser: Comonad]: P[StreamTypeToken[F]] =
    (`*`.lift ~ DataTypeToken.`datatypedef`[F]).map(ud => StreamTypeToken(ud._1, ud._2))

}

case class OptionTypeToken[F[_]: Comonad](override val unit: F[Unit], data: DataTypeToken[F])
    extends DataTypeToken[F] {
  override def as[T](v: T): F[T] = unit.as(v)
}

object OptionTypeToken {

  def `optiontypedef`[F[_]: LiftParser: Comonad]: P[OptionTypeToken[F]] =
    (`?`.lift ~ DataTypeToken.`datatypedef`[F]).map(ud => OptionTypeToken(ud._1, ud._2))

}

case class CustomTypeToken[F[_]: Comonad](name: F[String]) extends DataTypeToken[F] {
  override def as[T](v: T): F[T] = name.as(v)

  def value: String = name.extract
}

object CustomTypeToken {
  def ct[F[_]: LiftParser: Comonad]: P[CustomTypeToken[F]] = `Class`.lift.map(CustomTypeToken(_))
}

case class BasicTypeToken[F[_]: Comonad](scalarType: F[ScalarType]) extends DataTypeToken[F] {
  override def as[T](v: T): F[T] = scalarType.as(v)

  def value: ScalarType = scalarType.extract
}

object BasicTypeToken {

  def `basictypedef`[F[_]: LiftParser: Comonad]: P[BasicTypeToken[F]] =
    P.oneOf(
      ScalarType.all.map(n ⇒ P.string(n.name).as(n)).toList
    ).lift
      .map(BasicTypeToken(_))
}

sealed trait ArrowDef[F[_]] {
  def argTypes: List[TypeToken[F]]
  def resType: Option[DataTypeToken[F]]
}

case class ArrowTypeToken[F[_]: Comonad](
  override val unit: F[Unit],
  args: List[DataTypeToken[F]],
  res: Option[DataTypeToken[F]]
) extends TypeToken[F] with ArrowDef[F] {
  override def as[T](v: T): F[T] = unit.as(v)

  override def argTypes: List[TypeToken[F]] = args

  override def resType: Option[DataTypeToken[F]] = res
}

object ArrowTypeToken {

  def `arrowdef`[F[_]: LiftParser: Comonad]: P[ArrowTypeToken[F]] =
    (comma0(DataTypeToken.`datatypedef`).with1 ~ ` -> `.lift ~
      (DataTypeToken.`datatypedef`
        .map(Some(_)) | `()`.as(None))).map { case ((args, point), res) ⇒
      ArrowTypeToken(point, args, res)
    }

  def `arrowWithNames`[F[_]: LiftParser: Comonad]: P[ArrowTypeToken[F]] =
    (((`(`.lift <* `/s*`) ~ comma0(
      (Name.p[F] *> ` : ` *> DataTypeToken.`datatypedef`).surroundedBy(`/s*`)
    ) <* (`/s*` *> `)`)) ~
      (` -> ` *> DataTypeToken.`datatypedef`).?).map { case ((point, args), res) =>
      ArrowTypeToken(point, args, res)
    }
}

object DataTypeToken {

  def `arraytypedef`[F[_]: LiftParser: Comonad]: P[ArrayTypeToken[F]] =
    (`[]`.lift ~ `datatypedef`[F]).map(ud => ArrayTypeToken(ud._1, ud._2))

  def `topbottomdef`[F[_]: LiftParser: Comonad]: P[TopBottomToken[F]] =
    `⊥`.lift.map(TopBottomToken(_, isTop = false)) | `⊤`.lift.map(TopBottomToken(_, isTop = true))

  def `datatypedef`[F[_]: LiftParser: Comonad]: P[DataTypeToken[F]] =
    P.oneOf(
      P.defer(`arraytypedef`[F]) :: P.defer(StreamTypeToken.`streamtypedef`) :: P.defer(
        OptionTypeToken.`optiontypedef`
      ) :: BasicTypeToken
        .`basictypedef`[F] :: CustomTypeToken.ct[F] :: `topbottomdef` :: Nil
    )

}

object TypeToken {

  def `typedef`[F[_]: LiftParser: Comonad]: P[TypeToken[F]] =
    P.oneOf(
      ArrowTypeToken.`arrowdef`.backtrack :: DataTypeToken.`datatypedef` :: Nil
    )

}
