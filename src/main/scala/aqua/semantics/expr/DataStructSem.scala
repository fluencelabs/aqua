package aqua.semantics.expr

import aqua.model.Model
import aqua.parser.expr.DataStructExpr
import aqua.semantics.Prog
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.free.Free
import cats.syntax.functor._

class DataStructSem[F[_]](val expr: DataStructExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    Prog.after((_: Model) =>
      T.purgeFields(expr.name).flatMap {
        case Some(fields) => T.defineDataType(expr.name, fields) as Model.empty("Data struct makes no model")
        case None => Free.pure[Alg, Model](Model.error("Data struct types unresolved"))
      }
    )

}
