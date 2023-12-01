package aqua.semantics.expr

import aqua.parser.expr.ServiceExpr
import aqua.raw.{Raw, ServiceRaw}
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.definitions.DefinitionsAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra

import cats.data.EitherT
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.foldable.*
import cats.syntax.applicative.*
import cats.Monad

class ServiceSem[S[_]](val expr: ServiceExpr[S]) extends AnyVal {

  private def define[Alg[_]: Monad](using
    A: AbilitiesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg],
    D: DefinitionsAlgebra[S, Alg]
  ): EitherT[Alg, Raw, ServiceRaw] = for {
    arrows <- EitherT.fromOptionF(
      // TODO:  Move to purgeDefs here, allow not only arrows
      //        from parsing, throw errors here
      D.purgeArrows(expr.name),
      Raw.error("Service has no arrows")
    )
    arrowsByName = arrows.map { case (name, arrow) =>
      name.value -> (name, arrow)
    }.toNem
    defaultId <- expr.id.traverse(id =>
      EitherT.fromOptionF(
        V.valueToStringRaw(id),
        Raw.error("Failed to resolve default service id")
      )
    )
    serviceType <- EitherT.fromOptionF(
      T.defineServiceType(expr.name, arrowsByName.toSortedMap),
      Raw.error("Failed to define service type")
    )
    arrowsDefs = arrows.map { case (name, _) => name.value -> name }.toNem
    _ <- EitherT(
      A.defineService(
        expr.name,
        arrowsDefs,
        defaultId
      ).map(defined =>
        Raw
          .error("Service not created due to validation errors")
          .asLeft
          .whenA(!defined)
      )
    )
  } yield ServiceRaw(
    expr.name.value,
    serviceType,
    defaultId
  )

  def program[Alg[_]: Monad](using
    A: AbilitiesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg],
    D: DefinitionsAlgebra[S, Alg]
  ): Prog[Alg, Raw] = Prog.after_(
    define.value.map(_.merge)
  )
}
