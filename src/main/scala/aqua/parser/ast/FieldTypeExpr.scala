package aqua.parser.ast

import aqua.interim.types.TypesAlgebra
import aqua.parser.lexer.{DataTypeToken, Name}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import aqua.parser.lexer.Token._

case class FieldTypeExpr[F[_]](name: Name[F], `type`: DataTypeToken[F]) extends Expr[F] {

  def program[Alg[_]](implicit T: TypesAlgebra[F, Alg]): Prog[Alg, Unit] =
    for {
      t <- T.resolveType(`type`)
      _ <- T.defineField(name, t)
    } yield ()

}

object FieldTypeExpr extends Expr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[FieldTypeExpr[F]] =
    ((Name.p[F] <* ` : `) ~ DataTypeToken.`datatypedef`[F]).map {
      case (name, t) => FieldTypeExpr(name, t)
    }
}
