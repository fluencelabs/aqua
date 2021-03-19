package aqua.parser.expr

import aqua.semantics.Prog
import aqua.semantics.algebra.names.NamesAlgebra
import aqua.semantics.algebra.types.TypesAlgebra
import aqua.generator.Gen
import aqua.parser.Expr
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
    Prog.after((_: Gen) =>
      T.purgeFields(name).flatMap {
        case Some(fields) => T.defineDataType(name, fields) as Gen.noop // TODO it's not air gen, but ts gen
        case None => Gen.error.lift
      }
    )

}

object DataStructExpr extends Expr.AndIndented(FieldTypeExpr) {

  override def p[F[_]: LiftParser: Comonad]: Parser[DataStructExpr[F]] =
    `data` *> ` ` *> CustomTypeToken.ct[F].map(DataStructExpr(_)) <* ` : \n+`
}
