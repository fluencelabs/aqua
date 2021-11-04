package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Literal, Name, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser as P
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class ConstantExpr[F[_]](
  name: Name[F],
  value: Value[F],
  skipIfAlreadyDefined: Boolean
) extends Expr[F](ConstantExpr, name) {

  def mapK[K[_]: Comonad](fk: F ~> K): ConstantExpr[K] =
    copy(name.mapK(fk), value.mapK(fk), skipIfAlreadyDefined)
}

object ConstantExpr extends Expr.Leaf {
  
  private val constName: P[Name[Span.S]] =
    `const` *> ` ` *> Name.upper.withContext("Constant's names must be in UPPERCASE") <* ` `
  

  override val p: P[ConstantExpr[Span.S]] =
    (((constName ~ `?`.?).with1 <* `=` <* ` `) ~ Value.`value`)
    .map { case ((name, mark), value) =>
      ConstantExpr(name, value, mark.nonEmpty)
    }

  val onlyLiteral: P[(Name[Span.S], Literal[Span.S])] =
    ((((Name
      .upper <* ` `) ~ `?`.?).with1 <* `=` <* ` `) ~ Value.literal).map {
      case ((name, _), value) =>
        (name, value)
    }
}
