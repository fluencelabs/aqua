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

import aqua.helpers.data.SName
import aqua.parser.lexer.CallArrowToken.CallBraces
import aqua.parser.lexer.NamedArg.namedArgs
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}
import aqua.types.LiteralType

import cats.data.{NonEmptyList, NonEmptyMap}
import cats.parse.{Numbers, Parser as P, Parser0 as P0}
import cats.syntax.comonad.*
import cats.syntax.functor.*
import cats.{Comonad, Functor}
import cats.~>
import scala.language.postfixOps

sealed trait PropertyOp[F[_]] extends Token[F] {
  def mapK[K[_]: Comonad](fk: F ~> K): PropertyOp[K]
}

case class IntoArrow[F[_]: Comonad](name: Name[F], arguments: List[ValueToken[F]])
    extends PropertyOp[F] {
  override def as[T](v: T): F[T] = name.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): PropertyOp[K] =
    copy(name.mapK(fk), arguments.map(_.mapK(fk)))

  def simpleName: SName = SName.nameUnsafe(name.value)

  override def toString: String = s".$name(${arguments.map(_.toString).mkString(", ")})"
}

case class IntoField[F[_]: Comonad](name: F[String]) extends PropertyOp[F] {
  override def as[T](v: T): F[T] = name.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): PropertyOp[K] = copy(fk(name))

  lazy val value: String = name.extract

  def simpleName: SName = SName.nameUnsafe(value)

  override def toString: String = name.extract
}

case class IntoIndex[F[_]: Comonad](point: F[Unit], idx: Option[ValueToken[F]])
    extends PropertyOp[F] {
  override def as[T](v: T): F[T] = point.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): IntoIndex[K] = copy(fk(point), idx.map(_.mapK(fk)))

  override def toString: String = s"[$idx]"
}

case class IntoCopy[F[_]: Comonad](
  point: F[Unit],
  args: NonEmptyList[NamedArg[F]]
) extends PropertyOp[F] {
  override def as[T](v: T): F[T] = point.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): IntoCopy[K] =
    copy(fk(point), args.map(_.mapK(fk)))

  override def toString: String = s".copy(${args.map(_.toString).toList.mkString(", ")})"
}

/**
 * WARNING: This is parsed when we have parens after a name, but `IntoArrow` failed to parse.
 *          This is a case of imported named type, e.g. `Some.Imported.Module.DefinedAbility(...)`
 *          It is transformed into `NamedTypeValue` in `ValuesAlgebra`
 * TODO: Eliminate `IntoArrow`, unify it with this property
 */
case class IntoApply[F[_]: Comonad](
  argsF: F[NonEmptyList[NamedArg[F]]]
) extends PropertyOp[F] {
  lazy val args: NonEmptyList[NamedArg[F]] = argsF.extract

  override def as[T](v: T): F[T] = argsF.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): IntoApply[K] =
    copy(fk(argsF.map(_.map(_.mapK(fk)))))

  override def toString: String = s"(${args.map(_.toString).toList.mkString(", ")})"
}

object PropertyOp {

  private val parseField: P[PropertyOp[Span.S]] =
    `.` *> anyName.lift.map(IntoField(_))

  val parseArrow: P[PropertyOp[Span.S]] =
    (`.` *> CallArrowToken.callBraces).map { case CallBraces(name, abilities, args) =>
      IntoArrow(name, abilities ++ args)
    }

  val parseCopy: P[PropertyOp[Span.S]] =
    (`.` *> (`copy`.lift ~ namedArgs)).map(IntoCopy.apply)

  private val parseIdx: P[PropertyOp[Span.S]] =
    (P.defer(
      (ValueToken.`value`
        .surroundedBy(`/s*`)
        .between(`[`.between(` *`, `/s*`), `/s*` *> `]`)
        .lift | (exclamation *> ValueToken.num).lift)
        .map(v => IntoIndex(v.map(_.unit), Some(v._2)))
        .backtrack
    ) | exclamation.lift.map(e => IntoIndex(e, None))).flatMap { ii =>
      ii.idx match {
        case Some(LiteralToken(_, lt)) if lt == LiteralType.signed =>
          P.fail.withContext("Collection indexes must be non-negative")
        case _ => P.pure(ii)
      }
    }

  private val parseApply: P[PropertyOp[Span.S]] =
    namedArgs.lift.map(IntoApply.apply)

  private val parseOp: P[PropertyOp[Span.S]] =
    P.oneOf(
      // NOTE: order is important here
      // intoApply has lower priority than intoArrow
      parseCopy.backtrack ::
        parseArrow.backtrack ::
        parseField ::
        parseIdx ::
        parseApply.backtrack :: Nil
    )

  val ops: P[NonEmptyList[PropertyOp[Span.S]]] =
    parseOp.rep

}
