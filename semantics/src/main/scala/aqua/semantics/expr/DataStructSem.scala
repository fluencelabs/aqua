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
import cats.syntax.traverse.*
import cats.syntax.flatMap.*
import cats.Monad

class DataStructSem[S[_]](val expr: DataStructExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    D: DefinitionsAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog.after((_: Raw) =>
      for {
        defs <- D.purgeDefs()
        fields = defs.view.mapValues(d => d.name -> d.`type`).toMap
        structType <- T.defineStructType(expr.name, fields)
        result = structType.map(st => TypeRaw(expr.name.value, st))
      } yield result.getOrElse(Raw.error("Data struct types unresolved"))
    )

}
