package aqua.parser.lexer

import aqua.parser.{Expr, FunctorK}
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.types.LiteralType
import cats.parse.{Numbers, Parser as P}
import cats.syntax.comonad.*
import cats.syntax.functor.*
import cats.{Comonad, Functor}
import cats.~>

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

  def varLambda[F[_]: LiftParser: Comonad]: P[VarLambda[F]] =
    (Name.dotted[F] ~ LambdaOp.ops[F].?).map { case (n, l) ⇒
      VarLambda(n, l.fold[List[LambdaOp[F]]](Nil)(_.toList))
    }

  def bool[F[_]: LiftParser: Functor: Comonad]: P[Literal[F]] =
    P.oneOf(
      ("true" :: "false" :: Nil)
        .map(t ⇒ P.string(t).lift.map(fu => Literal(fu.as(t), LiteralType.bool)))
    ) <* P.not(`anum_*`)

  def initPeerId[F[_]: LiftParser: Comonad]: P[Literal[F]] =
    `%init_peer_id%`.string.lift.map(Literal(_, LiteralType.string))

  def num[F[_]: LiftParser: Comonad]: P[Literal[F]] =
    (P.char('-').?.with1 ~ Numbers.nonNegativeIntString).lift.map(fu =>
      fu.extract match {
        case (Some(_), n) ⇒ Literal(fu.as(s"-$n"), LiteralType.signed)
        case (None, n) ⇒ Literal(fu.as(n), LiteralType.number)
      }
    )

  def float[F[_]: LiftParser: Comonad]: P[Literal[F]] =
    (P.char('-').?.with1 ~ (Numbers.nonNegativeIntString <* P.char(
      '.'
    )) ~ Numbers.nonNegativeIntString).string.lift
      .map(Literal(_, LiteralType.float))

  // TODO make more sophisticated escaping/unescaping
  def string[F[_]: LiftParser: Comonad]: P[Literal[F]] =
    (`"` *> P.charsWhile0(_ != '"') <* `"`).string.lift
      .map(Literal(_, LiteralType.string))

  def literal[F[_]: LiftParser: Comonad]: P[Literal[F]] =
    P.oneOf(bool.backtrack :: float.backtrack :: num.backtrack :: string :: Nil)

  def `value`[F[_]: LiftParser: Comonad]: P[Value[F]] =
    P.oneOf(literal.backtrack :: initPeerId.backtrack :: varLambda :: Nil)

}
