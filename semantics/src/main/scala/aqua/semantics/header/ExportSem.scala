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

import aqua.helpers.data.{PName, SName}
import aqua.parser.head.*
import aqua.parser.lexer.{QName, Token}
import aqua.semantics.SemanticError
import aqua.semantics.header.HeaderHandler.*
import aqua.semantics.header.Picker.*
import aqua.semantics.rules.locations.LocationsAlgebra

import cats.data.*
import cats.data.Validated.*
import cats.instances.option.*
import cats.kernel.Semigroup
import cats.syntax.apply.*
import cats.syntax.bifunctor.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.semigroup.*
import cats.syntax.validated.*
import cats.{Comonad, Monoid}

class ExportSem[S[_]: Comonad, C: Monoid](expr: ExportExpr[S])(using
  picker: Picker[C],
  locations: LocationsAlgebra[S, State[C, *]]
) {

  private def exportAbilityCheck(
    ctx: C,
    token: Token[S],
    name: PName
  ): ValidatedNec[SemanticError[S], Unit] =
    Validated.condNec(
      !ctx.isAbility(name),
      (),
      error(
        token,
        s"Can not export '${name}' as it is an ability"
      )
    )

  private def exportFuncChecks(
    ctx: C,
    token: Token[S],
    name: PName
  ): ValidatedNec[SemanticError[S], Unit] =
    Validated.condNec(
      !ctx.funcReturnAbilityOrArrow(name),
      (),
      error(
        token,
        s"The function '$name' cannot be exported, because it returns an arrow, an ability or a stream map"
      )
    ) combine Validated.condNec(
      !ctx.funcAcceptAbility(name),
      (),
      error(
        token,
        s"The function '$name' cannot be exported, because it accepts an ability or a stream map"
      )
    )

  def headerSem: Res[S, C] =
    // Save exports, finally handle them
    HeaderSem
      .fromFin(finSem)
      .validNec

  private def finSem(ctx: C): ValidatedNec[SemanticError[S], C] = {
    val tokens = expr.pubs.toList.flatMap { case QName.As(name, rename) =>
      rename.map(name.toPName -> _).toList :+ (name.toPName, name)
    }.widen[(PName, Token[S])]

    val resCtx = ctx.addOccurences(tokens)

    expr.pubs.map { case QName.As(name, rename) =>
      val pName = name.toPName

      val symbol = resCtx
        .pick(pName)
        .toValidNec(
          error(
            name,
            s"No '${name.value}' declaration or import found, " +
              s"cannot export, available functions: ${resCtx.funcNames.mkString(", ")}"
          )
        )

      val simpleRename = rename.fold(
        pName.simple.toValidNec(
          error(
            name,
            s"Can not export '${name.value}' as it has path name, " +
              s"use 'export ${name.value} as' with simple name without dots"
          )
        )
      )(newName =>
        newName.toPName.simple.toValidNec(
          error(
            newName,
            s"Can not export '${name.value}' by path name '${newName.value}', use simple name without dots"
          )
        )
      )

      symbol *>
        exportAbilityCheck(resCtx, name, pName) *>
        exportFuncChecks(resCtx, name, pName) *>
        simpleRename.map(r => List(pName -> r))
    }.combineAll.map(exps => resCtx.setExports(resCtx.exports ++ exps))
  }
}
