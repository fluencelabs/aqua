package aqua.parser.lexer

import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan, S}
import aqua.types.ScalarType

import cats.Comonad
import cats.data.NonEmptyList
import cats.parse.{Accumulator0, Parser as P, Parser0 as P0}
import cats.syntax.comonad.*
import cats.syntax.functor.*
import cats.~>

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

object ArrayTypeToken {

  val `arraytypedef`: P[ArrayTypeToken[Span.S]] =
    (`[]`.lift ~ DataTypeToken.`datatypedef`).map(ud => ArrayTypeToken(ud._1, ud._2))
}

case class StreamTypeToken[S[_]: Comonad](override val unit: S[Unit], data: DataTypeToken[S])
    extends DataTypeToken[S] {
  override def as[T](v: T): S[T] = unit.as(v)
  override def mapK[K[_]: Comonad](fk: S ~> K): StreamTypeToken[K] = copy(fk(unit), data.mapK(fk))
}

object StreamTypeToken {

  val `streamtypedef`: P[StreamTypeToken[Span.S]] =
    (`*`.lift ~ DataTypeToken.`datatypedef`).map(ud => StreamTypeToken(ud._1, ud._2))

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

case class NamedTypeToken[F[_]: Comonad](name: F[String]) extends DataTypeToken[F] {
  override def as[T](v: T): F[T] = name.as(v)
  def asName: Name[F] = Name[F](name)

  override def mapK[K[_]: Comonad](fk: F ~> K): NamedTypeToken[K] = copy(fk(name))

  def value: String = name.extract

  override def toString: String = name.extract
}

object NamedTypeToken {

  val ct: P[NamedTypeToken[Span.S]] =
    `Class`.lift.map(NamedTypeToken(_))

  def dotted: P[NamedTypeToken[Span.S]] =
    `Class`.repSep(`.`).string.lift.map(NamedTypeToken(_))
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
      .map(BasicTypeToken.apply)
}

case class ArrowTypeToken[S[_]: Comonad](
  override val unit: S[Unit],
  args: List[(Option[Name[S]], TypeToken[S])],
  res: List[TypeToken[S]]
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

  def typeDef(): P[TypeToken[S]] =
    P.defer(TypeToken.`typedef`.between(`(`, `)`).backtrack | TypeToken.`typedef`)

  def returnDef(): P[List[TypeToken[S]]] = comma(
    typeDef().backtrack
  ).map(_.toList)

  // {SomeAb, SecondAb} for NamedTypeToken
  def abilities(): P0[List[(Option[Name[S]], NamedTypeToken[S])]] =
    (`{` *> comma(`Class`.surroundedBy(`/s*`).lift.map(s => Option(Name(s)) -> NamedTypeToken(s)))
      .map(_.toList) <* `}`).?.map(_.getOrElse(List.empty))

  def `arrowdef`(argTypeP: P[TypeToken[Span.S]]): P[ArrowTypeToken[Span.S]] =
    ((abilities() ~ comma0(argTypeP)).with1 ~ ` -> `.lift ~
      (returnDef().backtrack
        | `()`.as(Nil))).map { case (((abs, argsList), point), res) ⇒
      val args = argsList.map(Option.empty[Name[Span.S]] -> _)
      ArrowTypeToken(
        point,
        abs ++ args,
        res
      )
    }

  def `arrowWithNames`(argTypeP: P[TypeToken[Span.S]]): P[ArrowTypeToken[Span.S]] =
    (((` `.?.with1 *> abilities().with1 ~ `(`.lift <* `/s*`) ~ comma0(
      (Name.p.map(Option(_)) ~ (` : ` *> (argTypeP | argTypeP.between(`(`, `)`))))
        .surroundedBy(`/s*`)
    ) <* (`/s*` *> `)` <* ` `.?)) ~
      (` -> ` *> returnDef()).?).map { case (((abilities, point), args), res) =>
      ArrowTypeToken(point, abilities ++ args, res.toList.flatMap(_.toList))
    }
}

object DataTypeToken {

  val `topbottomdef`: P[TopBottomToken[Span.S]] =
    `⊥`.lift.map(TopBottomToken(_, isTop = false)) |
      `⊤`.lift.map(TopBottomToken(_, isTop = true))

  def `datatypedef`: P[DataTypeToken[Span.S]] =
    P.oneOf(
      P.defer(`topbottomdef`) ::
        P.defer(ArrayTypeToken.`arraytypedef`) ::
        P.defer(StreamTypeToken.`streamtypedef`) ::
        P.defer(OptionTypeToken.`optiontypedef`) ::
        BasicTypeToken.`basictypedef` ::
        NamedTypeToken.dotted :: Nil
    )

}

object TypeToken {

  val `typedef`: P[TypeToken[Span.S]] =
    P.oneOf(
      ArrowTypeToken.`arrowdef`(DataTypeToken.`datatypedef`).backtrack ::
        DataTypeToken.`datatypedef` :: Nil
    )

}
