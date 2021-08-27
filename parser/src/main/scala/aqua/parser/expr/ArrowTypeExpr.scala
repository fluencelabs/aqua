package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{ArrowTypeToken, DataTypeToken, Name}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import cats.~>

case class ArrowTypeExpr[F[_]](name: Name[F], `type`: ArrowTypeToken[F])
    extends Expr[F](ArrowTypeExpr, name) {
  def mapK[K[_]: Comonad](fk: F ~> K): ArrowTypeExpr[K] = copy(name.mapK(fk), `type`.mapK(fk))
}

object ArrowTypeExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[ArrowTypeExpr[F]] =
    (Name
      .p[F] ~ ((` : ` *> ArrowTypeToken.`arrowdef`[F](
      DataTypeToken.`datatypedef`[F]
    )) | ArrowTypeToken.`arrowWithNames`(DataTypeToken.`datatypedef`[F]))).map { case (name, t) =>
      ArrowTypeExpr(name, t)
    }
}
