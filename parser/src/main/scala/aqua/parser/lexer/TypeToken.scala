/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.parser.lexer

import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}
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

sealed trait BasicTypeToken[S[_]] extends TypeToken[S] {
  override def mapK[K[_]: Comonad](fk: S ~> K): BasicTypeToken[K]
}

case class TopBottomToken[S[_]: Comonad](override val unit: S[Unit], isTop: Boolean)
    extends BasicTypeToken[S] {
  override def as[T](v: T): S[T] = unit.as(v)
  def isBottom: Boolean = !isTop
  override def mapK[K[_]: Comonad](fk: S ~> K): TopBottomToken[K] = copy(fk(unit), isTop)
}

case class ArrayTypeToken[S[_]: Comonad](override val unit: S[Unit], data: BasicTypeToken[S])
    extends BasicTypeToken[S] {
  override def as[T](v: T): S[T] = unit.as(v)
  override def mapK[K[_]: Comonad](fk: S ~> K): ArrayTypeToken[K] = copy(fk(unit), data.mapK(fk))
}

object ArrayTypeToken {

  val `arraytypedef`: P[ArrayTypeToken[Span.S]] =
    (`[]`.lift ~ BasicTypeToken.`compositetypedef`).map(ud => ArrayTypeToken(ud._1, ud._2))
}

case class StreamTypeToken[S[_]: Comonad](override val unit: S[Unit], data: BasicTypeToken[S])
    extends BasicTypeToken[S] {
  override def as[T](v: T): S[T] = unit.as(v)
  override def mapK[K[_]: Comonad](fk: S ~> K): StreamTypeToken[K] = copy(fk(unit), data.mapK(fk))
}

object StreamTypeToken {

  val `streamtypedef`: P[StreamTypeToken[Span.S]] =
    (`*`.lift ~ BasicTypeToken.`compositetypedef`).map(ud => StreamTypeToken(ud._1, ud._2))

}

case class StreamMapTypeToken[S[_] : Comonad](override val unit: S[Unit], data: BasicTypeToken[S])
  extends BasicTypeToken[S] {
  override def as[T](v: T): S[T] = unit.as(v)

  override def mapK[K[_] : Comonad](fk: S ~> K): StreamMapTypeToken[K] = copy(fk(unit), data.mapK(fk))
}

object StreamMapTypeToken {

  val `streammaptypedef`: P[StreamMapTypeToken[Span.S]] =
    (`%`.lift ~ BasicTypeToken.`compositetypedef`).map(ud => StreamMapTypeToken(ud._1, ud._2))

}

case class OptionTypeToken[F[_]: Comonad](override val unit: F[Unit], data: BasicTypeToken[F])
    extends BasicTypeToken[F] {
  override def as[T](v: T): F[T] = unit.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): OptionTypeToken[K] =
    copy(fk(unit), data.mapK(fk))
}

object OptionTypeToken {

  val `optiontypedef`: P[OptionTypeToken[Span.S]] =
    (`?`.lift ~ BasicTypeToken.`compositetypedef`).map(ud => OptionTypeToken(ud._1, ud._2))

}

case class NamedTypeToken[F[_]: Comonad](name: F[String]) extends BasicTypeToken[F] {
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

case class ScalarTypeToken[F[_]: Comonad](scalarType: F[ScalarType]) extends BasicTypeToken[F] {
  override def as[T](v: T): F[T] = scalarType.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): ScalarTypeToken[K] =
    copy(fk(scalarType))

  def value: ScalarType = scalarType.extract
}

object ScalarTypeToken {

  val scalartypedef: P[ScalarTypeToken[Span.S]] =
    P.oneOf(
      ScalarType.all.map(n ⇒ P.string(n.name).as(n)).toList
    ).lift
      .map(ScalarTypeToken.apply)
}

case class ArrowTypeToken[S[_]: Comonad](
  override val unit: S[Unit],
  args: List[(Option[Name[S]], TypeToken[S])],
  res: List[TypeToken[S]],
  abilities: List[NamedTypeToken[S]] = Nil
) extends TypeToken[S] {
  override def as[T](v: T): S[T] = unit.as(v)

  override def mapK[K[_]: Comonad](fk: S ~> K): ArrowTypeToken[K] =
    copy(
      fk(unit),
      args.map { case (n, t) => (n.map(_.mapK(fk)), t.mapK(fk)) },
      res.map(_.mapK(fk)),
      abilities.map(_.mapK(fk))
    )
  def argTypes: List[TypeToken[S]] = abilities ++ args.map(_._2)

  lazy val absWithArgs: List[(Option[Name[S]], TypeToken[S])] =
    abilities.map(n => Some(n.asName) -> n) ++ args
}

object ArrowTypeToken {

  def typeDef(): P[TypeToken[S]] =
    P.defer(TypeToken.`typedef`.between(`(`, `)`).backtrack | TypeToken.`typedef`)

  def returnDef(): P[List[TypeToken[S]]] = comma(
    typeDef().backtrack
  ).map(_.toList)

  // {SomeAb, SecondAb} for NamedTypeToken
  def abilities(): P0[List[NamedTypeToken[S]]] = (
    `{` *> comma(NamedTypeToken.dotted).map(_.toList) <* `}`
  ).?.map(_.getOrElse(List.empty))

  def `arrowdef`(argTypeP: P[TypeToken[Span.S]]): P[ArrowTypeToken[Span.S]] =
    ((abilities() ~ comma0(argTypeP)).with1 ~ ` -> `.lift ~
      (returnDef().backtrack
        | `()`.as(Nil))).map { case (((abs, argsList), point), res) ⇒
      val args = argsList.map(Option.empty[Name[Span.S]] -> _)
      ArrowTypeToken(
        point,
        args,
        res,
        abs
      )
    }

  def `arrowWithNames`(argTypeP: P[TypeToken[Span.S]]): P[ArrowTypeToken[Span.S]] =
    (((` `.?.with1 *> abilities().with1 ~ `(`.lift <* `/s*`) ~ comma0(
      (Name.p.map(Option(_)) ~ (` : ` *> (argTypeP | argTypeP.between(`(`, `)`))))
        .surroundedBy(`/s*`)
    ) <* (`/s*` *> `)` <* ` `.?)) ~
      (` -> ` *> returnDef()).?).map { case (((abilities, point), args), res) =>
      ArrowTypeToken(point, args, res.toList.flatMap(_.toList), abilities)
    }
}

object BasicTypeToken {

  val `topbottomdef`: P[TopBottomToken[Span.S]] =
    `⊥`.lift.map(TopBottomToken(_, isTop = false)) |
      `⊤`.lift.map(TopBottomToken(_, isTop = true))

  def `compositetypedef`: P[BasicTypeToken[Span.S]] =
    P.oneOf(
      P.defer(`topbottomdef`) ::
        P.defer(ArrayTypeToken.`arraytypedef`) ::
        P.defer(StreamTypeToken.`streamtypedef`) ::
        P.defer(StreamMapTypeToken.`streammaptypedef`) ::
        P.defer(OptionTypeToken.`optiontypedef`) ::
        ScalarTypeToken.`scalartypedef` ::
        NamedTypeToken.dotted :: Nil
    )

}

object TypeToken {

  val `typedef`: P[TypeToken[Span.S]] =
    P.oneOf(
      ArrowTypeToken.`arrowdef`(BasicTypeToken.`compositetypedef`).backtrack ::
        BasicTypeToken.`compositetypedef` :: Nil
    )

}
