package aqua.ast.expr

import aqua.ast.{Expr, Gen, Prog}
import aqua.ast.algebra.names.NamesAlgebra
import aqua.ast.algebra.types.TypesAlgebra
import aqua.parser.lexer.CustomTypeToken
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.free.Free
import cats.parse.Parser
import cats.syntax.functor._

case class DataStructExpr[F[_]](name: CustomTypeToken[F]) extends Expr[F] {

  def program[Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg]
  ): Prog[Alg, Gen] =
    Prog after T
      .purgeFields(name)
      .flatMap {
        case Some(fields) => T.defineDataType(name, fields) as Gen(s"Data struct ${name.value} created")
        case None => Free.pure(Gen(s"Data struct ${name.value} can't be created"))
      }

}

object DataStructExpr extends Expr.AndIndented(FieldTypeExpr) {

  override def p[F[_]: LiftParser: Comonad]: Parser[DataStructExpr[F]] =
    `data` *> ` ` *> CustomTypeToken.ct[F].map(DataStructExpr(_)) <* ` : \n+`
}
