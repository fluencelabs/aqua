package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.PushToStreamExpr
import aqua.parser.expr.func.PushToStreamExpr.ValueOrPair
import aqua.parser.lexer.Token.*
import cats.syntax.either.*
import aqua.parser.lexer.{Name, ValueToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{*, given}
import cats.parse.Parser as P
import cats.{~>, Comonad}

case class PushToStreamExpr[F[_]](
  stream: Name[F],
  value: ValueOrPair[F]
) extends Expr[F](PushToStreamExpr, stream) {

  override def mapK[K[_]: Comonad](fk: F ~> K): PushToStreamExpr[K] =
    copy(stream.mapK(fk), value.bimap(p => (p._1.mapK(fk), p._2.mapK(fk)), v => v.mapK(fk)))
}

object PushToStreamExpr extends Expr.Leaf {

  type ValueOrPair[S[_]] = Either[(ValueToken[S], ValueToken[S]), ValueToken[S]]

  private val pair: P[(ValueToken[S], ValueToken[S])] = ` `.?.with1 ~ `(` ~ `/s*` *> ((ValueToken.`value` <* (` `.? ~ `,` ~ ` `.?)) ~ ValueToken.`value`) <* `/s*` ~ `)`

  private val valueOrPair: P[ValueOrPair[S]] = ValueToken.`value`.eitherOr(pair)

  override val p: P[PushToStreamExpr[Span.S]] =
    ((Name.p <* ` <<- `).with1 ~ valueOrPair).map {
      case (variable, value) => PushToStreamExpr(variable, value)
    }
}
