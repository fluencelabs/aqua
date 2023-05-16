package aqua.semantics.expr

import aqua.parser.expr.DataStructExpr
import aqua.raw.{Raw, TypeRaw}
import aqua.semantics.Prog
import aqua.semantics.rules.definitions.DefinitionsAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.StructType
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.Monad

class DataStructSem[S[_]](val expr: DataStructExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    D: DefinitionsAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog.after((_: Raw) =>
      D.purgeDefs(expr.name).flatMap {
        case Some(fields) =>
          val t = StructType(expr.name.value, fields)
          T.defineNamedType(expr.name, t).map {
            case true =>
              TypeRaw(
                expr.name.value,
                t
              ): Raw
            case false =>
              Raw.error("Data struct types unresolved")
          }
        case None => Raw.error("Data struct types unresolved").pure[Alg]
      }
    )

}
