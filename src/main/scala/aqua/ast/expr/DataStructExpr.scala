package aqua.ast.expr

import aqua.ast.{Expr, Prog}
import aqua.ast.algebra.names.NamesAlgebra
import aqua.ast.algebra.types.TypesAlgebra
import aqua.parser.lexer.CustomTypeToken
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import cats.syntax.comonad._

case class DataStructExpr[F[_]](name: CustomTypeToken[F]) extends Expr[F] {

  def program[Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg],
    F: Comonad[F]
  ): Prog[Alg, Unit] =
    Prog after T
      .purgeFields()
      .map(_.map(kv => kv._1.name.extract -> kv._2).toNem)
      .flatMap(T.defineDataType(name, _))

}

object DataStructExpr extends Expr.AndIndented(FieldTypeExpr) {

  override def p[F[_]: LiftParser: Comonad]: Parser[DataStructExpr[F]] =
    `data` *> ` ` *> CustomTypeToken.ct[F].map(DataStructExpr(_)) <* ` : \n+`
}
