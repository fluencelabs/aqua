package aqua.parser.lexer

import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.types.ScalarType
import cats.Comonad
import cats.parse.{Accumulator0, Parser as P}
import cats.syntax.comonad.*
import cats.syntax.functor.*
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

sealed trait TypeToken[S[_]] extends Token[S] {
  def mapK[K[_]: Comonad](fk: S ~> K): TypeToken[K]
}

sealed trait DataTypeToken[S[_]] extends TypeToken[S] {
  override def mapK[K[_]: Comonad](fk: S ~> K): DataTypeToken[K]
}

case class TopBottomToken[S[_]: Comonad](override val unit: S[Unit], isTop: Boolean)
    extends DataTypeToken[S] {
  override def as[T](v: T): S[T] = unit.as(v)
  def isBottom: Boolean = !isTop
  override def mapK[K[_]: Comonad](fk: S ~> K): TopBottomToken[K] = copy(fk(unit), isTop)
}

case class ArrayTypeToken[S[_]: Comonad](override val unit: S[Unit], data: DataTypeToken[S])
    extends DataTypeToken[S] {
  override def as[T](v: T): S[T] = unit.as(v)
  override def mapK[K[_]: Comonad](fk: S ~> K): ArrayTypeToken[K] = copy(fk(unit), data.mapK(fk))
}

case class StreamTypeToken[S[_]: Comonad](override val unit: S[Unit], data: DataTypeToken[S])
    extends DataTypeToken[S] {
  override def as[T](v: T): S[T] = unit.as(v)
  override def mapK[K[_]: Comonad](fk: S ~> K): StreamTypeToken[K] = copy(fk(unit), data.mapK(fk))
}

object StreamTypeToken {

  val `streamtypedef`: P[StreamTypeToken[Span.S]] =
    ((`*`.lift <* P.not(`*`).withContext("Nested streams '**type' is prohibited"))
      ~ DataTypeToken.`withoutstreamdatatypedef`)
      .map(ud => StreamTypeToken(ud._1, ud._2))

}

case class OptionTypeToken[F[_]: Comonad](override val unit: F[Unit], data: DataTypeToken[F])
    extends DataTypeToken[F] {
  override def as[T](v: T): F[T] = unit.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): OptionTypeToken[K] =
    copy(fk(unit), data.mapK(fk))
}

object OptionTypeToken {

  val `optiontypedef`: P[OptionTypeToken[Span.S]] =
    (`?`.lift ~ DataTypeToken.`datatypedef`).map(ud => OptionTypeToken(ud._1, ud._2))

}

case class CustomTypeToken[F[_]: Comonad](name: F[String]) extends DataTypeToken[F] {
  override def as[T](v: T): F[T] = name.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): CustomTypeToken[K] = copy(fk(name))

  def value: String = name.extract
}

object CustomTypeToken {

  val ct: P[CustomTypeToken[Span.S]] =
    `Class`.lift.map(CustomTypeToken(_))

  def dotted: P[CustomTypeToken[Span.S]] =
    `Class`.repSep(`.`).string.lift.map(CustomTypeToken(_))
}

case class BasicTypeToken[F[_]: Comonad](scalarType: F[ScalarType]) extends DataTypeToken[F] {
  override def as[T](v: T): F[T] = scalarType.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): BasicTypeToken[K] =
    copy(fk(scalarType))
  def value: ScalarType = scalarType.extract
}

object BasicTypeToken {

  val `basictypedef`: P[BasicTypeToken[Span.S]] =
    P.oneOf(
      ScalarType.all.map(n ⇒ P.string(n.name).as(n)).toList
    ).lift
      .map(BasicTypeToken(_))
}

case class ArrowTypeToken[S[_]: Comonad](
  override val unit: S[Unit],
  args: List[(Option[Name[S]], TypeToken[S])],
  res: List[DataTypeToken[S]]
) extends TypeToken[S] {
  override def as[T](v: T): S[T] = unit.as(v)

  override def mapK[K[_]: Comonad](fk: S ~> K): ArrowTypeToken[K] =
    copy(
      fk(unit),
      args.map { case (n, t) => (n.map(_.mapK(fk)), t.mapK(fk)) },
      res.map(_.mapK(fk))
    )
  def argTypes: List[TypeToken[S]] = args.map(_._2)
}

object ArrowTypeToken {

  def `arrowdef`(argTypeP: P[TypeToken[Span.S]]): P[ArrowTypeToken[Span.S]] =
    (comma0(argTypeP).with1 ~ ` -> `.lift ~
      (comma(DataTypeToken.`datatypedef`).map(_.toList)
        | `()`.as(Nil))).map { case ((args, point), res) ⇒
      ArrowTypeToken(point, args.map(Option.empty[Name[Span.S]] -> _), res)
    }

  def `arrowWithNames`(argTypeP: P[TypeToken[Span.S]]): P[ArrowTypeToken[Span.S]] =
    (((` `.?.with1 *> `(`.lift <* `/s*`) ~ comma0(
      (Name.p.map(Option(_)) ~ (` : ` *> (argTypeP | argTypeP.between(`(`, `)`))))
        .surroundedBy(`/s*`)
    ) <* (`/s*` *> `)` <* ` `.?)) ~
      (` -> ` *> comma(DataTypeToken.`datatypedef`)).?).map { case ((point, args), res) =>
      ArrowTypeToken(point, args, res.toList.flatMap(_.toList))
    }
}

object DataTypeToken {

  val `arraytypedef`: P[ArrayTypeToken[Span.S]] =
    (`[]`.lift ~ `datatypedef`).map(ud => ArrayTypeToken(ud._1, ud._2))

  val `topbottomdef`: P[TopBottomToken[Span.S]] =
    `⊥`.lift.map(TopBottomToken(_, isTop = false)) |
      `⊤`.lift.map(TopBottomToken(_, isTop = true))

  val `withoutstreamdatatypedef`: P[DataTypeToken[Span.S]] =
    P.oneOf(
      P.defer(`topbottomdef`) :: P.defer(`arraytypedef`) :: P.defer(
        OptionTypeToken.`optiontypedef`
      ) :: BasicTypeToken.`basictypedef` :: CustomTypeToken.dotted :: Nil
    )

  def `datatypedef`: P[DataTypeToken[Span.S]] =
    P.oneOf(
      P.defer(`topbottomdef`) :: P.defer(`arraytypedef`) :: P.defer(
        StreamTypeToken.`streamtypedef`
      ) :: P.defer(
        OptionTypeToken.`optiontypedef`
      ) :: BasicTypeToken.`basictypedef` :: CustomTypeToken.dotted :: Nil
    )

}

object TypeToken {

  val `typedef`: P[TypeToken[Span.S]] =
    P.oneOf(
      ArrowTypeToken
        .`arrowdef`((DataTypeToken.`datatypedef`))
        .backtrack :: DataTypeToken.`datatypedef` :: Nil
    )

}
