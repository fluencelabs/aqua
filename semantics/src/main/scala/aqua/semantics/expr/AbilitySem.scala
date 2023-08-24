package aqua.semantics.expr

import aqua.parser.expr.AbilityExpr
import aqua.raw.{Raw, ServiceRaw, TypeRaw}
import aqua.parser.lexer.{Name, NamedTypeToken}
import aqua.raw.{Raw, ServiceRaw}
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.definitions.DefinitionsAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{AbilityType, ArrowType, Type}
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.semigroupal.*
import cats.syntax.traverse.*
import cats.Monad
import cats.data.{NonEmptyList, NonEmptyMap}

class AbilitySem[S[_]](val expr: AbilityExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    T: TypesAlgebra[S, Alg],
    D: DefinitionsAlgebra[S, Alg]
  ): Prog[Alg, Raw] = {
    Prog.after_(
      for {
        defs <- D.purgeDefs(expr.name)
        fields = defs.view.mapValues(d => d.name -> d.`type`).toMap
        abilityType <- T.defineAbilityType(expr.name, fields)
        result = abilityType.map(st => TypeRaw(expr.name.value, st))
      } yield result.getOrElse(Raw.error("Ability types unresolved"))
    )
  }
}
