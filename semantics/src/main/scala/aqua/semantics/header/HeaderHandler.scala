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

import aqua.errors.Errors.internalError
import aqua.helpers.data.PName
import aqua.parser.Ast
import aqua.parser.head.*
import aqua.parser.lexer.QName
import aqua.parser.lexer.{Ability, Token}
import aqua.raw.RawContext
import aqua.semantics.header.Picker.*
import aqua.semantics.rules.locations.LocationsAlgebra
import aqua.semantics.{HeaderError, SemanticError}

import cats.data.*
import cats.data.Validated.*
import cats.syntax.bifunctor.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.semigroup.*
import cats.syntax.validated.*
import cats.{Comonad, Monoid}

class HeaderHandler[S[_]: Comonad, C](using
  acm: Monoid[C],
  headMonoid: Monoid[HeaderSem[S, C]],
  picker: Picker[C],
  // NOTE: This typeclass is here to reuse
  // the code from the body semantics
  locations: LocationsAlgebra[S, State[C, *]]
) {

  import HeaderHandler.*

  def sem(imports: Map[String, C], header: Ast.Head[S]): Res[S, C] = {
    // Resolve a filename from given imports or fail
    def resolve(f: FilenameExpr[S]): ResAC[S, C] =
      imports
        .get(f.fileValue)
        .map(_.pickDeclared)
        .toValidNec(
          error(f.token, "Cannot resolve the import")
        )

    // Get part of the declared context (for import/use ... from ... expressions)
    def getFrom(f: FromExpr[S], ctx: C): ResAC[S, C] =
      ctx.pickHeader.validNec |+| f.imports.map { case QName.As(importName, importRename) =>
        val piName = importName.toPName
        val piRename = importRename.map(_.toPName)
        val name = ctx.moduleName.fold(piName)(piName.prepended)
        val rename = piRename.map(r => ctx.moduleName.fold(r)(r.prepended))

        ctx
          .pick(name, rename)
          .map { ctx =>
            val defName = rename.getOrElse(name)
            val occs = importRename.map(defName -> _).toList :+ (defName, importName)

            ctx.addOccurences(occs)
          }
          .toValidNec(
            error(
              importName,
              s"Imported file `declares ${ctx.declares.map(_.value).mkString(", ")}`, " +
                s"no ${name.value} declared. Try adding `declares ${name.value}` to that file."
            )
          )
      }.combineAll

    // Convert an imported context into a module (ability)
    def toModule(ctx: C, tkn: Token[S], rename: Option[PName]): ResAC[S, C] =
      ctx.moduleName
        .toValidNec(
          error(
            tkn,
            s"Used module has no `aqua` header. Please add `aqua` header or `use ... as ModuleName`, or switch to import"
          )
        )
        .map { modName =>
          rename
            .filter(_ != modName)
            .fold(ctx.clearModule)(newName =>
              ctx
                .pick(modName, newName.some)
                .getOrElse(
                  internalError(s"Module ${modName.value} does not contain itself")
                )
            )
        }

    def unscope(ctx: C, tkn: Token[S]): ResAC[S, C] =
      ctx.moduleName
        .toValidNec(
          error(
            tkn,
            s"Used module has no `aqua` header. Please add `aqua` header or `use ... as ModuleName`, or switch to import"
          )
        )
        .map { modName =>
          ctx
            .unscoped(modName)
            .getOrElse(
              internalError(s"Module ${modName.value} does not contain itself")
            )
        }

    val handleModule: ModuleExpr[S] => Res[S, C] = { me =>
      ModuleSem(me).headerSem
    }

    // Handler for every header expression, will be combined later
    val onExpr: HeaderExpr[S] => Res[S, C] = {
      case m: ModuleExpr[S] =>
        error(m.token, "Module header is expected to be at the top").invalidNec

      case f @ ImportExpr(_) =>
        // Import everything from a file
        resolve(f)
          .andThen(unscope(_, f.token))
          .map(HeaderSem.fromInit)

      case f @ ImportFromExpr(_, _) =>
        // Import, map declarations
        resolve(f)
          .andThen(getFrom(f, _))
          .andThen(unscope(_, f.token))
          .map(HeaderSem.fromInit)

      case f @ UseExpr(_, asModule) =>
        // Import, move into a module scope
        resolve(f)
          .andThen(ctx =>
            toModule(
              ctx = ctx,
              tkn = f.token,
              rename = asModule.map(_.toPName)
            )
          )
          .map(HeaderSem.fromInit)

      case f @ UseFromExpr(_, _, asModule) =>
        // Import, cherry-pick declarations, move to a module scope
        resolve(f)
          .andThen(getFrom(f, _))
          .andThen(ctx =>
            toModule(
              ctx = ctx,
              tkn = f.token,
              rename = asModule.map(_.toPName)
            )
          )
          .map(HeaderSem.fromInit)

      case ee: ExportExpr[S] =>
        ExportSem(ee).headerSem
    }

    val (module, other) =
      header.headers.uncons.collect { case (m: ModuleExpr[S], rest) =>
        (m.some, rest)
      }.getOrElse((none, header.headers))
        .bimap(
          _.toValidNec(
            error(
              header.begin,
              "Missing module header at the top of the file"
            )
          ).andThen(handleModule),
          _.foldMap(onExpr)
        )

    other |+| module
  }
}

object HeaderHandler {

  type Res[S[_], C] = ValidatedNec[SemanticError[S], HeaderSem[S, C]]
  type ResAC[S[_], C] = ValidatedNec[SemanticError[S], C]

  // Error generator with token pointer
  def error[S[_], T](
    token: Token[S],
    msg: String
  ): SemanticError[S] = HeaderError(token, msg)
}
