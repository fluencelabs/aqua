package aqua.semantics.expr

import aqua.parser.expr.DataStructExpr
import aqua.raw.{Raw, TypeRaw}
import aqua.semantics.Prog
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.StructType
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.Monad

class DataStructSem[S[_]](val expr: DataStructExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog.after((_: Raw) =>
      T.purgeDefs(expr.name).flatMap {
        case Some(fields) =>
          val t = StructType(expr.name.value, fields)
          T.defineType(expr.name, t) as (TypeRaw(
            expr.name.value,
            t
          ): Raw)
        case None => Raw.error("Data struct types unresolved").pure[Alg]
      }
    )

}
