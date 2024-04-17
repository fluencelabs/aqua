package aqua.semantics.expr

import aqua.parser.expr.AbilityExpr
import aqua.raw.{ErroredPart, Raw, TypeRaw}
import aqua.semantics.Prog
import aqua.semantics.rules.definitions.DefinitionsAlgebra
import aqua.semantics.rules.types.TypesAlgebra

import cats.Monad
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class AbilitySem[S[_]](val expr: AbilityExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    T: TypesAlgebra[S, Alg],
    D: DefinitionsAlgebra[S, Alg]
  ): Prog[Alg, Raw] = {
    Prog.after_(
      for {
        defs <- D.purgeDefs()
        fields = defs.view.mapValues(d => d.name -> d.`type`).toMap
        abilityType <- T.defineAbilityType(expr.name, fields)
        result = abilityType.map(st => TypeRaw(expr.name.value, st))
      } yield result.getOrElse(ErroredPart(expr.name.value))
    )
  }
}
