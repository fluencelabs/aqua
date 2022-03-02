package aqua.parser.lexer

import aqua.parser.Expr
import aqua.parser.head.FilenameExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.types.LiteralType
import cats.parse.{Numbers, Parser as P, Parser0 as P0}
import cats.syntax.comonad.*
import cats.syntax.functor.*
import cats.{~>, Comonad, Functor}
import cats.data.NonEmptyList
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

sealed trait ValueToken[F[_]] extends Token[F] {
  def mapK[K[_]: Comonad](fk: F ~> K): ValueToken[K]
}

case class VarToken[F[_]](name: Name[F], lambda: List[LambdaOp[F]] = Nil) extends ValueToken[F] {
  override def as[T](v: T): F[T] = name.as(v)

  def mapK[K[_]: Comonad](fk: F ~> K): VarToken[K] = copy(name.mapK(fk), lambda.map(_.mapK(fk)))
}

case class LiteralToken[F[_]: Comonad](valueToken: F[String], ts: LiteralType)
    extends ValueToken[F] {
  override def as[T](v: T): F[T] = valueToken.as(v)

  def mapK[K[_]: Comonad](fk: F ~> K): LiteralToken[K] = copy(fk(valueToken), ts)

  def value: String = valueToken.extract
}

case class CollectionToken[F[_]: Comonad](values: NonEmptyList[ValueToken[F]])
    extends ValueToken[F] {
  override def mapK[K[_]: Comonad](fk: F ~> K): ValueToken[K] = copy(values.map(_.mapK(fk)))

  override def as[T](v: T): F[T] = values.head.as(v)
}

object ValueToken {

  val varLambda: P[VarToken[Span.S]] =
    (Name.dotted ~ LambdaOp.ops.?).map { case (n, l) ⇒
      VarToken(n, l.fold[List[LambdaOp[Span.S]]](Nil)(_.toList))
    }

  val bool: P[LiteralToken[Span.S]] =
    P.oneOf(
      ("true" :: "false" :: Nil)
        .map(t ⇒ P.string(t).lift.map(fu => LiteralToken(fu.as(t), LiteralType.bool)))
    ) <* P.not(`anum_*`)

  val initPeerId: P[LiteralToken[Span.S]] =
    `%init_peer_id%`.string.lift.map(LiteralToken(_, LiteralType.string))

  val minus = P.char('-')
  val dot = P.char('.')

  val num: P[LiteralToken[Span.S]] =
    (minus.?.with1 ~ Numbers.nonNegativeIntString).lift.map(fu =>
      fu.extract match {
        case (Some(_), n) ⇒ LiteralToken(fu.as(s"-$n"), LiteralType.signed)
        case (None, n) ⇒ LiteralToken(fu.as(n), LiteralType.number)
      }
    )

  val float: P[LiteralToken[Span.S]] =
    (minus.?.with1 ~ (Numbers.nonNegativeIntString <* dot) ~ Numbers.nonNegativeIntString).string.lift
      .map(LiteralToken(_, LiteralType.float))

  val charsWhileQuotes = P.charsWhile0(_ != '"')

  // TODO make more sophisticated escaping/unescaping
  val string: P[LiteralToken[Span.S]] =
    (`"` *> charsWhileQuotes <* `"`).string.lift
      .map(LiteralToken(_, LiteralType.string))

  val literal: P[LiteralToken[Span.S]] =
    P.oneOf(bool.backtrack :: float.backtrack :: num.backtrack :: string :: Nil)

  def collection: P[CollectionToken[Span.S]] =
    (`[` *> P.defer(`_value`).repSep(`,`) <* `]`).map(CollectionToken(_))

  def `_value`: P[ValueToken[Span.S]] =
    P.oneOf(literal.backtrack :: initPeerId.backtrack :: P.defer(collection) :: varLambda :: Nil)

  val `value`: P[ValueToken[Span.S]] =
    P.defer(`_value`)

}
