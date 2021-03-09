package aqua.parser.lexer

import aqua.parser.lift.LiftParser
import cats.parse.{Parser => P}
import Token._

case class Arg[F[_]](name: Name[F], `type`: TypeToken[F])

object Arg {

  def p[F[_]: LiftParser]: P[Arg[F]] =
    ((Name.p[F] <* ` : `) ~ TypeToken.`typedef`[F]).map {
      case (name, t) => Arg(name, t)
    }
}
