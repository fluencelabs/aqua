package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.*
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser as P
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}
import aqua.parser.lexer.PrefixToken
import aqua.parser.lexer.VarToken

case class ConstantExpr[F[_]](
  name: Name[F],
  value: ValueToken[F],
  skipIfAlreadyDefined: Boolean
) extends Expr[F](ConstantExpr, name) {

  def mapK[K[_]: Comonad](fk: F ~> K): ConstantExpr[K] =
    copy(name.mapK(fk), value.mapK(fk), skipIfAlreadyDefined)
}

object ConstantExpr extends Expr.Leaf {

  private val constName: P[Name[Span.S]] =
    `const` *> ` ` *> Name.upper.withContext("Constant's names must be in UPPERCASE") <* ` `

  override val p: P[ConstantExpr[Span.S]] =
    (((constName ~ `?`.?).with1 <* `=` <* ` `) ~ ValueToken.`value`).flatMap {
      case ((name, mark), value) =>
        lazy val fail = (what: String) =>
          P.failWith(
            s"'$name' is $what, but only strings, numbers or booleans can be used"
          )
        value match {
          case CollectionToken(point, _) => fail("a collection")
          case CallArrowToken(_, _, _) => fail("a function call")
          case InfixToken(_, _, _) | PrefixToken(_, _) => fail("an expression")
          case PropertyToken(_, _) => fail("a property")
          case NamedValueToken(_, _) => fail("an ability or data")
          case LiteralToken(_, _) | VarToken(_) =>
            P.pure(ConstantExpr(name, value, mark.nonEmpty))
        }

    }

  val onlyLiteral: P[(Name[Span.S], LiteralToken[Span.S])] =
    ((((Name.upper <* ` `) ~ `?`.?).with1 <* `=` <* ` `) ~ ValueToken.literal).map {
      case ((name, _), value) =>
        (name, value)
    }
}
