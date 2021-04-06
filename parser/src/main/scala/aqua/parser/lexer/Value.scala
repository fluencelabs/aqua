package aqua.parser.lexer

import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import aqua.types.LiteralType
import cats.{Comonad, Functor}
import cats.parse.{Numbers, Parser => P}
import cats.syntax.functor._
import cats.syntax.comonad._

sealed trait Value[F[_]] extends Token[F]

case class VarLambda[F[_]](name: Name[F], lambda: List[LambdaOp[F]] = Nil) extends Value[F] {
  override def as[T](v: T): F[T] = name.as(v)
}

case class Literal[F[_]: Comonad](valueToken: F[String], ts: LiteralType) extends Value[F] {
  override def as[T](v: T): F[T] = valueToken.as(v)

  def value: String = valueToken.extract
}

object Value {

  def varLambda[F[_]: LiftParser: Comonad]: P[VarLambda[F]] =
    (Name.p[F] ~ LambdaOp.ops[F].?).map { case (n, l) ⇒
      VarLambda(n, l.fold[List[LambdaOp[F]]](Nil)(_.toList))
    }

  def bool[F[_]: LiftParser: Functor: Comonad]: P[Literal[F]] =
    P.oneOf(
      ("true" :: "false" :: Nil)
        .map(t ⇒ P.string(t).lift.map(fu => Literal(fu.as(t), LiteralType.bool)))
    )

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
    P.oneOf(bool :: float.backtrack :: num :: string :: Nil)

  def `value`[F[_]: LiftParser: Comonad]: P[Value[F]] =
    P.oneOf(literal.backtrack :: varLambda :: Nil)

}
