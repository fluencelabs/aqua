package aqua.semantics.expr

import aqua.model.{Model, TypeModel}
import aqua.parser.expr.DataStructExpr
import aqua.semantics.Prog
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.StructType
import cats.syntax.functor._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.Monad

class DataStructSem[F[_]](val expr: DataStructExpr[F]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    Prog.after((_: Model) =>
      T.purgeFields(expr.name).flatMap {
        case Some(fields) =>
          T.defineDataType(expr.name, fields) as (TypeModel(
            expr.name.value,
            StructType(expr.name.value, fields)
          ): Model)
        case None => Model.error("Data struct types unresolved").pure[Alg]
      }
    )

}
