package aqua.semantics.expr

import aqua.parser.expr.ScopeExpr
import aqua.parser.lexer.{CustomTypeToken, Name}
import aqua.raw.{Raw, ServiceRaw}
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrowType, Type}
import aqua.raw.ScopeRaw
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.Monad
import cats.data.NonEmptyList

class ScopeSem[S[_]](val expr: ScopeExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    A: AbilitiesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog.around(
      A.beginScope(expr.name),
      (_: Unit, _: Raw) =>
        Raw.error("Undefined").pure[Alg])
}
