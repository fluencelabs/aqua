package aqua.semantics.expr

import aqua.parser.expr.AbilityExpr
import aqua.raw.{Raw, ScopeRaw, ServiceRaw, TypeRaw}
import aqua.parser.lexer.{Name, NamedTypeToken}
import aqua.raw.{Raw, ServiceRaw}
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.definitions.DefinitionsAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrowType, AbilityType, Type}
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.semigroupal.*
import cats.Monad
import cats.data.{NonEmptyList, NonEmptyMap}

class AbilitySem[S[_]](val expr: AbilityExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    T: TypesAlgebra[S, Alg],
    D: DefinitionsAlgebra[S, Alg]
  ): Prog[Alg, Raw] = {
    Prog.after(_ =>
      D.purgeDefs(expr.name).flatMap {
        case Some(fields) =>
          val t = AbilityType(expr.name.value, fields)
          T.defineNamedType(expr.name, t).map {
            case true =>
              TypeRaw(
                expr.name.value,
                t
              ): Raw
            case false =>
              Raw.error("Scope types unresolved")
          }
        case None => Raw.error("Scope types unresolved").pure[Alg]
      }
    )
  }
}
