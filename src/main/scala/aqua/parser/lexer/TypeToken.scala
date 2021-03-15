package aqua.parser.lexer

import aqua.ast.algebra.types.ScalarType
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.{Comonad, Functor}
import cats.parse.{Parser => P}
import cats.syntax.functor._
import cats.syntax.comonad._

sealed trait TypeToken[F[_]] extends Token[F]
sealed trait DataTypeToken[F[_]] extends TypeToken[F]

case class ArrayTypeToken[F[_]: Comonad](override val unit: F[Unit], data: DataTypeToken[F]) extends DataTypeToken[F] {
  override def as[T](v: T): F[T] = unit.as(v)
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
      )
      .lift
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

  def `arraytypedef`[F[_]: LiftParser: Comonad]: P[ArrayTypeToken[F]] =
    (P.string("[]").lift ~ `datatypedef`[F]).map(ud => ArrayTypeToken(ud._1, ud._2))

  def `datatypedef`[F[_]: LiftParser: Comonad]: P[DataTypeToken[F]] =
    P.oneOf(P.defer(`arraytypedef`[F]) :: BasicTypeToken.`basictypedef`[F] :: CustomTypeToken.ct[F] :: Nil)
}

object TypeToken {

  def `typedef`[F[_]: LiftParser: Comonad]: P[TypeToken[F]] =
    P.oneOf(ArrowTypeToken.`arrowdef`.backtrack :: DataTypeToken.`datatypedef` :: Nil)

}
