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

package aqua.lsp

import aqua.parser.Ast
import aqua.parser.head.{ImportExpr, ImportFromExpr, UseExpr, UseFromExpr}
import aqua.parser.lexer.LiteralToken
import aqua.raw.ConstantRaw
import aqua.semantics.*
import aqua.semantics.header.Picker.*
import aqua.semantics.rules.locations.LocationsState

import cats.data.EitherT
import cats.syntax.applicative.*
import cats.syntax.semigroup.*
import monocle.Lens
import monocle.macros.GenLens

class LspSemantics[S[_]](
  constants: List[ConstantRaw] = Nil
) extends Semantics[S, LspContext[S]] {

  private def getImportTokens(ast: Ast[S]): List[LiteralToken[S]] =
    ast.head.collect {
      case ImportExpr(fn) => fn
      case ImportFromExpr(_, fn) => fn
      case UseExpr(fn, _) => fn
      case UseFromExpr(_, fn, _) => fn
    }.toList

  /**
   * Process the AST and return the semantics result.
   * NOTE: LspSemantics never return errors or warnings,
   * they are collected in LspContext.
   */
  def process(
    ast: Ast[S],
    init: LspContext[S]
  ): ProcessResult = {

    val withConstants = init.addFreeParts(constants)
    val rawState = CompilerState.init[S](withConstants.raw)

    val initState = rawState.copy(
      locations = rawState.locations.copy(
        variables = rawState.locations.variables |+| withConstants.variables
      )
    )

    val importTokens = getImportTokens(ast)

    given Lens[CompilerState[S], LocationsState[S]] =
      GenLens[CompilerState[S]](_.locations)

    given LocationsInterpreter[S, CompilerState[S]] =
      new LocationsInterpreter[S, CompilerState[S]]()

    RawSemantics
      .interpret(ast, withConstants.raw)
      .run(initState)
      .map { case (state, ctx) =>
        LspContext(
          raw = ctx,
          importTokens = importTokens,
          variables = state.locations.variables,
          errors = state.errors.toList,
          warnings = state.warnings.toList
        ).pure[Result]
      }
      .value
  }
}
