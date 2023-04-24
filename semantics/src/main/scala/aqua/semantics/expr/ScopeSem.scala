package aqua.semantics.expr

import aqua.parser.expr.ScopeExpr
import aqua.parser.lexer.{CustomTypeToken, Name}
import aqua.raw.{Raw, ScopeRaw, ServiceRaw, TypeRaw}
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrowType, ScopeType, Type}
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.semigroupal.*
import cats.Monad
import cats.data.{NonEmptyList, NonEmptyMap}

class ScopeSem[S[_]](val expr: ScopeExpr[S]) extends AnyVal {

  def arrowsToNem(arrows: NonEmptyList[(Name[S], ArrowType)]): NonEmptyMap[String, Type] =
    arrows.map(kv => (kv._1.value, kv._2)).toNem

  def program[Alg[_]: Monad](implicit
    A: AbilitiesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog.around(
      A.beginScope(expr.name),
      (_, _) => T.purgeDefs(expr.name).mapN {
        case (None, None) =>
          None
        case (Some(arrows), None) =>
          Option(ScopeType(expr.name.value, arrowsToNem(arrows)))
        case (None, Some(fields)) =>
          Option(ScopeType(expr.name.value, fields))
        case (Some(arrows), Some(fields)) =>
          Option(ScopeType(expr.name.value, arrowsToNem(arrows) ++ fields))
      }.flatMap {
        case Some(t) =>
          T.defineType(expr.name, t) as TypeRaw(expr.name.value, t)
        case None =>
          Raw.error("Scope has no arrows and fields, fails").pure[Alg]
      } <* A.endScope()
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
