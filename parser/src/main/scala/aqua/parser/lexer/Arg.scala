package aqua.parser.lexer

import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import Token._
import cats.Comonad
import cats.parse.{Parser => P}

case class Arg[F[_]](name: Name[F], `type`: TypeToken[F])

object Arg {

  val p: P[Arg[Span.S]] =
    ((Name.p <* ` : `) ~ TypeToken.`typedef`).map { case (name, t) =>
      Arg(name, t)
    }
}
