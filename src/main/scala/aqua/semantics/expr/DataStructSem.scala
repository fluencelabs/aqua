package aqua.semantics.expr

import aqua.generator.Gen
import aqua.parser.expr.DataStructExpr
import aqua.semantics.Prog
import aqua.semantics.algebra.names.NamesAlgebra
import aqua.semantics.algebra.types.TypesAlgebra
import cats.syntax.functor._

class DataStructSem[F[_]](val expr: DataStructExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg]
  ): Prog[Alg, Gen] =
    Prog.after((_: Gen) =>
      T.purgeFields(expr.name).flatMap {
        case Some(fields) => T.defineDataType(expr.name, fields) as Gen.noop // TODO it's not air gen, but ts gen
        case None => Gen.error.lift
      }
    )

}
