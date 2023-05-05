package aqua.semantics.expr

import aqua.parser.expr.ArrowTypeExpr
import aqua.raw.{Raw, TypeRaw}
import aqua.semantics.Prog
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.definitions.DefinitionsAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.Monad

class ArrowTypeSem[S[_]](val expr: ArrowTypeExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    T: TypesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg],
    D: DefinitionsAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    T.resolveArrowDef(expr.`type`).flatMap {
      case Some(t) => D.defineArrow(expr.name, t) as (TypeRaw(expr.name.value, t): Raw)
      case None => Raw.error("Arrow type unresolved").pure[Alg]
    }

}
