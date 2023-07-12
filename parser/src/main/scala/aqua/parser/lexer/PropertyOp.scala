package aqua.parser.lexer

import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import cats.data.{NonEmptyList, NonEmptyMap}
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

case class IntoArrow[F[_]: Comonad](name: Name[F], arguments: List[ValueToken[F]]) extends PropertyOp[F] {
  override def as[T](v: T): F[T] = name.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): PropertyOp[K] = copy(name.mapK(fk), arguments.map(_.mapK(fk)))

  override def toString: String = s".$name(${arguments.map(_.toString).mkString(", ")})"
}

case class IntoField[F[_]: Comonad](name: F[String]) extends PropertyOp[F] {
  override def as[T](v: T): F[T] = name.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): PropertyOp[K] = copy(fk(name))

  def value: String = name.extract

  override def toString: String = name.extract
}

case class IntoIndex[F[_]: Comonad](point: F[Unit], idx: Option[ValueToken[F]])
    extends PropertyOp[F] {
  override def as[T](v: T): F[T] = point.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): IntoIndex[K] = copy(fk(point), idx.map(_.mapK(fk)))
}

case class IntoCopy[F[_]: Comonad](point: F[Unit], fields: NonEmptyMap[String, ValueToken[F]])
    extends PropertyOp[F] {
  override def as[T](v: T): F[T] = point.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): IntoCopy[K] =
    copy(fk(point), fields.map(_.mapK(fk)))
}

object PropertyOp {

  private val parseField: P[PropertyOp[Span.S]] =
    (`.` *> `name`).lift.map(IntoField(_))

  val parseArrow: P[PropertyOp[Span.S]] =
    (`.` *> CallArrowToken.callBraces()).lift.map(p => IntoArrow(p._2._1, p._2._2 ++ p._2._3))

  val parseCopy: P[PropertyOp[Span.S]] =
    (`.` *> (`copy`.lift ~ namedArgs)).map { case (point, fields) =>
      IntoCopy(point, NonEmptyMap.of(fields.head, fields.tail: _*))
    }

  private val parseIdx: P[PropertyOp[Span.S]] =
    (P.defer(
      (ValueToken.`value`.surroundedBy(`/s*`).between(`[`.between(` *`, `/s*`), `/s*` *> `]`).lift | (exclamation *> ValueToken.num).lift)
        .map(v => IntoIndex(v.map(_.unit), Some(v._2)))
        .backtrack
    ) | exclamation.lift.map(e => IntoIndex(e, None))).flatMap { ii =>
      ii.idx match {
        case Some(LiteralToken(_, lt)) if lt == LiteralType.signed =>
          P.fail.withContext("Collection indexes must be non-negative")
        case _ => P.pure(ii)
      }
    }

  private val parseOp: P[PropertyOp[Span.S]] =
    P.oneOf(parseCopy.backtrack :: parseField.backtrack :: parseIdx :: Nil)

  val opsWithArrows: P[NonEmptyList[PropertyOp[Span.S]]] =
    P.oneOf(parseCopy.backtrack :: parseArrow.backtrack :: parseField.backtrack :: parseIdx :: Nil).rep

  val ops: P[NonEmptyList[PropertyOp[Span.S]]] =
    parseOp.rep

}
