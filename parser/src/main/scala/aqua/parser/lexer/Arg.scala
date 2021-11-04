package aqua.parser.lexer

import aqua.parser.lift.LiftParser
import cats.parse.{Parser => P}
import Token._
import cats.Comonad
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class Arg[F[_]](name: Name[F], `type`: TypeToken[F])

object Arg {

  val p: P[Arg[Span.S]] =
    ((Name.p <* ` : `) ~ TypeToken.`typedef`).map { case (name, t) =>
      Arg(name, t)
    }
}
