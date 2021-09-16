package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Literal, Name, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.{Parser => P}
import cats.~>

case class ConstantExpr[F[_]](
  name: Name[F],
  value: Value[F],
  skipIfAlreadyDefined: Boolean
) extends Expr[F](ConstantExpr, name) {

  def mapK[K[_]: Comonad](fk: F ~> K): ConstantExpr[K] =
    copy(name.mapK(fk), value.mapK(fk), skipIfAlreadyDefined)
}

object ConstantExpr extends Expr.Leaf {
  
  private def constName[F[_]: LiftParser: Comonad]: P[Name[F]] =
    `const` *> ` ` *> Name.upper[F].withContext("Constant's names must be in UPPERCASE") <* ` `
  

  override def p[F[_]: LiftParser: Comonad]: P[ConstantExpr[F]] =
    (((constName ~ `?`.?).with1 <* `=` <* ` `) ~ Value.`value`)
    .map { case ((name, mark), value) =>
      ConstantExpr(name, value, mark.nonEmpty)
    }

  def onlyLiteral[F[_]: LiftParser: Comonad]: P[(Name[F], Literal[F])] =
    ((((Name
      .upper[F] <* ` `) ~ `?`.?).with1 <* `=` <* ` `) ~ Value.literal).map {
      case ((name, _), value) =>
        (name, value)
    }
}
