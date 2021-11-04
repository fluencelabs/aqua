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
import cats.{Comonad, Functor, ~>}
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

sealed trait Value[F[_]] extends Token[F] {
  def mapK[K[_]: Comonad](fk: F ~> K): Value[K]
}

case class VarLambda[F[_]](name: Name[F], lambda: List[LambdaOp[F]] = Nil) extends Value[F] {
  override def as[T](v: T): F[T] = name.as(v)

  def mapK[K[_]: Comonad](fk: F ~> K): VarLambda[K] = copy(name.mapK(fk), lambda.map(_.mapK(fk)))
}

case class Literal[F[_]: Comonad](valueToken: F[String], ts: LiteralType) extends Value[F] {
  override def as[T](v: T): F[T] = valueToken.as(v)

  def mapK[K[_]: Comonad](fk: F ~> K): Literal[K] = copy(fk(valueToken), ts)

  def value: String = valueToken.extract
}

object Value {

  val varLambda: P[VarLambda[Span.S]] =
    (Name.dotted ~ LambdaOp.ops.?).map { case (n, l) ⇒
      VarLambda(n, l.fold[List[LambdaOp[Span.S]]](Nil)(_.toList))
    }

  val bool: P[Literal[Span.S]] =
    P.oneOf(
      ("true" :: "false" :: Nil)
        .map(t ⇒ P.string(t).lift.map(fu => Literal(fu.as(t), LiteralType.bool)))
    ) <* P.not(`anum_*`)

  val initPeerId: P[Literal[Span.S]] =
    `%init_peer_id%`.string.lift.map(Literal(_, LiteralType.string))

  val minus = P.char('-')
  val dot = P.char('.')

  val num: P[Literal[Span.S]] =
    (minus.?.with1 ~ Numbers.nonNegativeIntString).lift.map(fu =>
      fu.extract match {
        case (Some(_), n) ⇒ Literal(fu.as(s"-$n"), LiteralType.signed)
        case (None, n) ⇒ Literal(fu.as(n), LiteralType.number)
      }
    )

  val float: P[Literal[Span.S]] =
    (minus.?.with1 ~ (Numbers.nonNegativeIntString <* dot) ~ Numbers.nonNegativeIntString).string.lift
      .map(Literal(_, LiteralType.float))

  val charsWhileQuotes = P.charsWhile0(_ != '"')

  // TODO make more sophisticated escaping/unescaping
  val string: P[Literal[Span.S]] =
    (`"` *> charsWhileQuotes <* `"`).string.lift
      .map(Literal(_, LiteralType.string))

  val literal: P[Literal[Span.S]] =
    P.oneOf(bool.backtrack :: float.backtrack :: num.backtrack :: string :: Nil)

  val `value`: P[Value[Span.S]] =
    P.oneOf(literal.backtrack :: initPeerId.backtrack :: varLambda :: Nil)

}
