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

import aqua.compiler.{AquaCompiler, AquaCompilerConf, AquaError, AquaSources}
import aqua.parser.{Ast, ParserError}
import aqua.raw.{ConstantRaw, RawContext}
import aqua.semantics.FileId
import aqua.semantics.header.{HeaderHandler, HeaderSem}
import aqua.semantics.rules.locations.LocationsAlgebra

import cats.data.Validated.validNec
import cats.data.{Chain, State, Validated, ValidatedNec}
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.monoid.*
import cats.syntax.semigroup.*
import cats.{Comonad, Monad, Monoid, Order, Show}

object LSPCompiler {

  private def getLspAquaCompiler[F[_]: Monad, E, I: FileId, S[_]: Comonad](
    config: AquaCompilerConf
  ): AquaCompiler[F, E, I, S, LspContext[S]] = {
    given LocationsAlgebra[S, State[LspContext[S], *]] =
      LocationsInterpreter[S, LspContext[S]]()

    val constants = config.constants ++ ConstantRaw.defaultConstants(config.relayVarName)

    new AquaCompiler(
      headerHandler = new HeaderHandler(),
      semantics = new LspSemantics(constants)
    )
  }

  def compileToLsp[F[_]: Monad, E, I: FileId, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]],
    config: AquaCompilerConf
  ): F[ValidatedNec[AquaError[I, E, S], Map[I, LspContext[S]]]] = {

    val compiler = getLspAquaCompiler[F, E, I, S](config)
    compiler
      .compileRaw(sources, parser)
      // NOTE: Ignore warnings here as
      // they are collected inside context
      .map(_.value.value.toValidated)
  }
}
