/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.semantics.expr

import aqua.helpers.syntax.optiont.withFilterF
import aqua.parser.expr.ServiceExpr
import aqua.raw.{ErroredPart, Raw, ServiceRaw}
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.definitions.DefinitionsAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra

import cats.Monad
import cats.data.{EitherT, OptionT}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*

class ServiceSem[S[_]](val expr: ServiceExpr[S]) extends AnyVal {

  private def define[Alg[_]: Monad](using
    A: AbilitiesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg],
    D: DefinitionsAlgebra[S, Alg]
  ): EitherT[Alg, Raw, ServiceRaw] = {
    (
      for {
        arrows <- OptionT(D.purgeArrows(expr.name))
        arrowsByName = arrows.map { case (name, arrow) =>
          name.value -> (name, arrow)
        }.toNem
        defaultId <- expr.id.traverse(id => OptionT(V.valueToStringRaw(id)))
        serviceType <- OptionT(T.defineServiceType(expr.name, arrowsByName.toSortedMap))
        arrowsDefs = arrows.map { case (name, _) => name.value -> name }.toNem
        _ <- OptionT.withFilterF(
          A.defineService(
            expr.name,
            arrowsDefs,
            defaultId
          )
        )
      } yield ServiceRaw(
        expr.name.value,
        serviceType,
        defaultId
      )
    ).toRight(ErroredPart(expr.name.value))
  }

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
