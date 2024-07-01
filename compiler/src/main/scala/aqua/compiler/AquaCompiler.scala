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

package aqua.compiler

import aqua.compiler.AquaError.*
import aqua.linker.Linker
import aqua.parser.{Ast, ParserError}
import aqua.semantics.header.Picker.setImportPaths
import aqua.semantics.header.{HeaderHandler, Picker}
import aqua.semantics.{FileId, SemanticError, Semantics}

import cats.arrow.FunctionK
import cats.data.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.show.*
import cats.{Comonad, Monad, Monoid, Order, Show, ~>}
import scribe.Logging

class AquaCompiler[F[_]: Monad, E, I: FileId, S[_]: Comonad, C: Picker](
  headerHandler: HeaderHandler[S, C],
  semantics: Semantics[S, C]
) extends Logging {

  type Err = AquaError[I, E, S]

  type CompileWarns = [A] =>> CompileWarnings[S][A]
  type CompileRes = [A] =>> CompileResult[I, E, S][A]

  // Transpilation function for module
  // (Imports contexts => Compilation result)
  type TP = Map[String, C] => CompileRes[C]

  private def transpile(body: Ast[S], importPaths: Map[String, String]): TP =
    imports =>
      for {
        // Process header, get initial context
        headerSem <- headerHandler
          .sem(imports, body.head)
          .toCompileRes
        // Analyze the body, with prepared initial context
        _ = logger.trace("semantic processing...")
        processed <- semantics
          .process(body, headerSem.init)
          .toCompileRes
        // Handle exports, declares - finalize the resulting context
        rc <- headerSem
          .fin(processed)
          .toCompileRes
      } yield rc.setImportPaths(importPaths)

  def compileRaw(
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]]
  ): F[CompileRes[Map[I, C]]] = {
    logger.trace("starting resolving sources...")

    val parsing = new AquaParser(sources, parser)

    parsing.resolve.value.map(resolution =>
      for {
        // Lift resolution to CompileRes
        modules <- resolution.toEitherT[CompileWarns]
        // Generate transpilation functions for each module
        transpiled = modules.map { m =>
          val importPaths = m.imports.view.mapValues(_.show).toMap
          m.copy(body = transpile(m.body, importPaths))
        }
        // Link modules
        linked <- Linker.link(transpiled, CycleError.apply)
      } yield linked
    )
  }

  private val warningsK: semantics.Warnings ~> CompileWarns =
    new FunctionK[semantics.Warnings, CompileWarns] {

      override def apply[A](
        fa: semantics.Warnings[A]
      ): CompileWarns[A] =
        fa.mapWritten(_.map(AquaWarning.CompileWarning.apply))
    }

  extension (res: semantics.ProcessResult) {

    def toCompileRes: CompileRes[C] =
      res
        .leftMap(_.map(CompileError.apply))
        .mapK(warningsK)
  }

  extension [A](res: ValidatedNec[SemanticError[S], A]) {

    def toCompileRes: CompileRes[A] =
      res.toEither
        .leftMap(_.map(CompileError.apply))
        .toEitherT[CompileWarns]
  }

}
