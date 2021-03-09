package aqua.parser.lexer

import aqua.interim.types.ScalarType
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Functor
import cats.parse.{Parser => P}
import cats.syntax.functor._

sealed trait TypeToken[F[_]] extends Token[F]
sealed trait DataTypeToken[F[_]] extends TypeToken[F]

// TODO add F[Unit]
case class ArrayTypeToken[F[_]](data: DataTypeToken[F]) extends DataTypeToken[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = data.as(v)
}

case class CustomTypeToken[F[_]](name: F[String]) extends DataTypeToken[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = name.as(v)
}

object CustomTypeToken {
  def ct[F[_]: LiftParser]: P[CustomTypeToken[F]] = `Class`.lift.map(CustomTypeToken(_))
}

case class BasicTypeToken[F[_]](value: F[ScalarType]) extends DataTypeToken[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = value.as(v)
}

object BasicTypeToken {

  def `basictypedef`[F[_]: LiftParser]: P[BasicTypeToken[F]] =
    P.oneOf(
        ScalarType.all.map(n ⇒ P.string(n.name).as(n)).toList
      )
      .lift
      .map(BasicTypeToken(_))
}

sealed trait ArrowDef[F[_]] {
  def argTypes: List[TypeToken[F]]
  def resType: Option[DataTypeToken[F]]
}

case class ArrowTypeToken[F[_]](point: F[Unit], args: List[DataTypeToken[F]], res: Option[DataTypeToken[F]])
    extends TypeToken[F] with ArrowDef[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = point.as(v)

  override def argTypes: List[TypeToken[F]] = args

  override def resType: Option[DataTypeToken[F]] = res
}

object ArrowTypeToken {

  def `arrowdef`[F[_]: LiftParser]: P[ArrowTypeToken[F]] =
    (comma0(DataTypeToken.`datatypedef`).with1 ~ `->`.lift ~
      (DataTypeToken.`datatypedef`
        .map(Some(_)) | P.string("()").as(None))).map {
      case ((args, point), res) ⇒ ArrowTypeToken(point, args, res)
    }
}

case class AquaArrowType[F[_]](args: List[TypeToken[F]], res: Option[DataTypeToken[F]]) extends ArrowDef[F] {
  override def argTypes: List[TypeToken[F]] = args

  override def resType: Option[DataTypeToken[F]] = res
}

object DataTypeToken {

  def `arraytypedef`[F[_]: LiftParser]: P[ArrayTypeToken[F]] =
    (P.string("[]") *> `datatypedef`[F]).map(ArrayTypeToken(_))

  def `datatypedef`[F[_]: LiftParser]: P[DataTypeToken[F]] =
    P.oneOf(P.defer(`arraytypedef`[F]) :: BasicTypeToken.`basictypedef`[F] :: CustomTypeToken.ct[F] :: Nil)
}

object TypeToken {

  def `typedef`[F[_]: LiftParser]: P[TypeToken[F]] =
    P.oneOf(ArrowTypeToken.`arrowdef`.backtrack :: DataTypeToken.`datatypedef` :: Nil)

}
