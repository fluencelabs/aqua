package aqua.parser.lexer

import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import cats.data.NonEmptyList
import cats.parse.{Numbers, Parser as P, Parser0 as P0}
import cats.syntax.comonad.*
import cats.syntax.functor.*
import cats.{Comonad, Functor}

import scala.language.postfixOps
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}
import aqua.types.LiteralType

sealed trait PropertyOp[F[_]] extends Token[F] {
  def mapK[K[_]: Comonad](fk: F ~> K): PropertyOp[K]
}

case class IntoField[F[_]: Comonad](name: F[String]) extends PropertyOp[F] {
  override def as[T](v: T): F[T] = name.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): PropertyOp[K] = copy(fk(name))

  def value: String = name.extract

  override def toString: String = name.extract
}

case class IntoIndex[F[_]: Comonad](token: Token[F], idx: Option[ValueToken[F]])
    extends PropertyOp[F] {
  override def as[T](v: T): F[T] = token.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): IntoIndex[K] =
    copy(token.mapK(fk), idx.map(_.mapK(fk)))
}

object PropertyOp {

  private val parseField: P[PropertyOp[Span.S]] =
    (`.` *> `name`).lift.map(IntoField(_))

  private val parseIdx: P[PropertyOp[Span.S]] =
    (P.defer(
      (ValueToken.`value`.between(`[`, `]`).lift | (exclamation *> ValueToken.num).lift)
        .map(v => IntoIndex(Token.lift[Span.S, ValueToken[Span.S]](v), Some(v._2)))
        .backtrack
    ) | exclamation.lift.map(e => IntoIndex(Token.lift[Span.S, Unit](e), None))).flatMap { ii =>
      ii.idx match {
        case Some(LiteralToken(_, lt)) if lt == LiteralType.signed =>
          P.fail.withContext("Collection indexes must be non-negative")
        case _ => P.pure(ii)
      }
    }

  private val parseOp: P[PropertyOp[Span.S]] =
    P.oneOf(parseField.backtrack :: parseIdx :: Nil)

  val ops: P[NonEmptyList[PropertyOp[Span.S]]] =
    parseOp.rep

}
