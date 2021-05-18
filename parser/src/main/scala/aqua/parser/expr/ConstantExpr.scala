package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Name, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.{Parser => P}

case class ConstantExpr[F[_]](
  name: Name[F],
  value: Value[F],
  skipIfAlreadyDefined: Boolean
) extends Expr[F]

object ConstantExpr extends Expr.RootLeaf {

  override def p[F[_]: LiftParser: Comonad]: P[ConstantExpr[F]] = {
    ((((`const` *> ` ` *> Name
      .p[F] <* ` `) ~ `?`.?).with1 <* `=` <* ` `) ~ Value.`value`).map {
      case ((name, mark), value) =>
        ConstantExpr(name, value, mark.nonEmpty)
    }
  }
}
