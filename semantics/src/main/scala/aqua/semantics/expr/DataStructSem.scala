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

import aqua.parser.expr.DataStructExpr
import aqua.raw.{ErroredPart, Raw, TypeRaw}
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
      } yield result.getOrElse(ErroredPart(expr.name.value))
    )

}
