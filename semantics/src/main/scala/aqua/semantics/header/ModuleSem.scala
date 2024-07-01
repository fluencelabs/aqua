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

package aqua.semantics.header

import aqua.helpers.data.PName
import aqua.parser.head.ModuleExpr
import aqua.semantics.header.HeaderHandler.{Res, error}
import aqua.semantics.header.Picker.*
import aqua.semantics.rules.locations.LocationsAlgebra

import cats.data.*
import cats.data.Validated.*
import cats.kernel.Semigroup
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.semigroup.*
import cats.syntax.validated.*
import cats.{Comonad, Monoid}

class ModuleSem[S[_]: Comonad, C: Picker](expr: ModuleExpr[S])(using
  locations: LocationsAlgebra[S, State[C, *]]
) {

  def headerSem: Res[S, C] = {
    lazy val sem = HeaderSem(
      // Save module header info
      Picker[C].blank.setModule(expr.name.value.some),
      ctx =>
        expr.declares match {
          case None => ctx.validNec
          case Some(ModuleExpr.Declares.All(_)) =>
            val names = ctx.allNames.map(PName.simpleUnsafe).toSet
            ctx.setDeclares(names).validNec
          case Some(ModuleExpr.Declares.Names(declareNames)) =>
            val declares = declareNames.fproductLeft(_.value).toList
            val names = declareNames.map(_.toPName).toList.toSet
            val res = ctx.setDeclares(names).addOccurences(declares)

            // summarize contexts to allow redeclaration of imports
            declares.map { case (n, t) =>
              res
                .pick(t.toPName, ctx.module.nonEmpty)
                .toValidNec(
                  error(
                    t,
                    s"`$n` is expected to be declared, but declaration is not found in the file"
                  )
                )
                .void
            // TODO: Should not it be possible to make `.combineAll` the final result?
            // Seems like `.pick` does not return much information
            }.combineAll.as(res)
        }
    )

    expr.word.value.fold(
      module = error(
        expr.word,
        "Keyword `module` is deprecated, use `aqua` instead"
      ).invalidNec,
      aqua = sem.validNec
    )
  }
}
