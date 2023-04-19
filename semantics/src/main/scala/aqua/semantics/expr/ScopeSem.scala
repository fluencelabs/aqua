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
        Raw.error("Scope has no arrows and fields, fails").pure[Alg]
        /*(A.purgeArrows(expr.name), T.purgeFields(expr.name.value, expr.name)).flatMapN {
          case (None, None) =>
            Raw.error("Scope has no arrows and fields, fails").pure[Alg]
          case (Some(arrows), Some(fields)) =>
            A.defineScope(expr.name, fields) as (ScopeRaw(expr.name.value, fields): Raw)
          case (None, Some(fields)) => A.defineScope(expr.name, fields) as (ScopeRaw(expr.name.value, fields): Raw)
//          case (Some(arrows), None) => A.defineScope(expr.name, arrows.toNem.mapKeys(_.name)) as (ScopeRaw(expr.name.value, fields): Raw)
          case (_, _) =>
            Raw.error("Scope has no arrows and fields, fails").pure[Alg]

        } <* A.endScope()*/
    )

  /*private def defineArrows[Alg[_]: Monad](arrowsOp: Option[NonEmptyList[(Name[S], ArrowType)]])(
    implicit
    A: AbilitiesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ) = arrowsOp match {
    case Some(nel) =>
      val arrows = nel.map(kv => kv._1.value -> (kv._1, kv._2)).toNem
      for {
        defineResult <- A.defineScope(
          expr.name,
          arrows
        )
      } yield
        if (defineResult) {
          ScopeRaw(expr.name.value, arrows.map(_._2), defaultId)
        } else Raw.empty("Scope not created due to validation errors")
    case None =>

  }

  private def defineFields[Alg[_]: Monad](arrows: Option[NonEmptyList[(String, Type)]])(implicit
    A: AbilitiesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ) = {}*/
}
