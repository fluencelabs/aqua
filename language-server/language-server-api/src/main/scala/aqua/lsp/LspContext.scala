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

import aqua.helpers.data.PName
import aqua.parser.lexer.{LiteralToken, NamedTypeToken, Token}
import aqua.raw.{RawContext, RawPart}
import aqua.semantics.header.Picker
import aqua.semantics.rules.locations.LocationsState.*
import aqua.semantics.rules.locations.{LocationsState, TokenLocation, VariableInfo, Variables}
import aqua.semantics.{SemanticError, SemanticWarning}
import aqua.types.{AbilityType, ArrowType, Type}

import cats.syntax.monoid.*
import cats.syntax.semigroup.*
import cats.{Monoid, Semigroup}
import monocle.Lens
import scala.collection.immutable.ListMap

// Context with info that necessary for language server
case class LspContext[S[_]](
  raw: RawContext,
  abDefinitions: Map[String, NamedTypeToken[S]] = Map.empty[String, NamedTypeToken[S]],
  rootArrows: Map[String, ArrowType] = Map.empty[String, ArrowType],
  constants: Map[String, Type] = Map.empty[String, Type],
  // TODO: Can this field be refactored into LocationsState?
  variables: Variables[S] = Variables[S](),
  importTokens: List[LiteralToken[S]] = Nil,
  errors: List[SemanticError[S]] = Nil,
  warnings: List[SemanticWarning[S]] = Nil,
  importPaths: Map[String, String] = Map.empty
) {
  lazy val allLocations: List[TokenLocation[S]] = variables.allLocations

  lazy val tokenPaths: List[TokenImportPath[S]] = TokenImportPath.importPathsFromContext(this)
}

object LspContext {

  def blank[S[_]]: LspContext[S] = LspContext[S](raw = RawContext())

  given [S[_]]: Monoid[LspContext[S]] with {
    override def empty: LspContext[S] = blank[S]

    override def combine(x: LspContext[S], y: LspContext[S]): LspContext[S] =
      LspContext[S](
        raw = x.raw |+| y.raw,
        abDefinitions = x.abDefinitions ++ y.abDefinitions,
        rootArrows = x.rootArrows ++ y.rootArrows,
        constants = x.constants ++ y.constants,
        importTokens = x.importTokens ++ y.importTokens,
        variables = x.variables |+| y.variables,
        errors = x.errors ++ y.errors,
        warnings = x.warnings ++ y.warnings,
        importPaths = x.importPaths ++ y.importPaths
      )
  }

  given [S[_]]: Picker[LspContext[S]] with {
    import aqua.semantics.header.Picker.*

    override def blank: LspContext[S] = LspContext.blank[S]
    override def exports(ctx: LspContext[S]): Map[String, Option[String]] = ctx.raw.exports

    override def isAbility(ctx: LspContext[S], name: String): Boolean =
      ctx.raw.isAbility(name)

    override def funcReturnAbilityOrArrow(ctx: LspContext[S], name: String): Boolean =
      ctx.raw.funcReturnAbilityOrArrow(name)

    override def funcAcceptAbility(ctx: LspContext[S], name: String): Boolean =
      ctx.raw.funcAcceptAbility(name)

    override def funcNames(ctx: LspContext[S]): Set[String] = ctx.raw.funcNames

    override def definedAbilityNames(ctx: LspContext[S]): Set[String] =
      ctx.raw.definedAbilityNames

    override def addPart(ctx: LspContext[S], part: (LspContext[S], RawPart)): LspContext[S] =
      ctx.copy(raw = ctx.raw.addPart(part._1.raw -> part._2))

    override def module(ctx: LspContext[S]): Option[String] = ctx.raw.module

    override def declaredNames(ctx: LspContext[S]): Set[String] = ctx.raw.declaredNames

    override def allNames(ctx: LspContext[S]): Set[String] = ctx.raw.allNames

    override def setAbility(
      ctx: LspContext[S],
      name: String,
      ctxAb: LspContext[S]
    ): LspContext[S] =
      ctx.copy(
        raw = ctx.raw.setAbility(name, ctxAb.raw),
        variables = ctx.variables |+| ctxAb.variables.renameDefinitions(defName =>
          AbilityType.fullName(name, defName)
        )
      )

    override def setImportPaths(
      ctx: LspContext[S],
      importPaths: Map[String, String]
    ): LspContext[S] =
      ctx.copy(importPaths = importPaths)

    override def setModule(ctx: LspContext[S], name: Option[String]): LspContext[S] =
      ctx.copy(raw = ctx.raw.setModule(name))

    override def setDeclares(
      ctx: LspContext[S],
      declares: Set[PName]
    ): LspContext[S] =
      ctx.copy(raw = ctx.raw.setDeclares(declares))

    override def setExports(
      ctx: LspContext[S],
      exports: Map[String, Option[String]]
    ): LspContext[S] =
      ctx.copy(raw = ctx.raw.setExports(exports))

    override def pick(
      ctx: LspContext[S],
      name: String,
      rename: Option[String],
      declared: Boolean
    ): Option[LspContext[S]] =
      // rename tokens from one context with prefix addition
      val newVariables = rename.map { renameStr =>
        ctx.variables.renameDefinitions {
          case defName if defName.startsWith(name) =>
            defName.replaceFirst(name, renameStr)
        }
      }.getOrElse(ctx.variables)

      ctx.raw
        .pick(name, rename, declared)
        .map(rc =>
          ctx.copy(
            raw = rc,
            abDefinitions =
              ctx.abDefinitions.get(name).fold(Map.empty)(t => Map(rename.getOrElse(name) -> t)),
            rootArrows =
              ctx.rootArrows.get(name).fold(Map.empty)(t => Map(rename.getOrElse(name) -> t)),
            constants =
              ctx.constants.get(name).fold(Map.empty)(t => Map(rename.getOrElse(name) -> t)),
            variables = newVariables
          )
        )

    override def pick(ctx: LspContext[S], name: PName, declared: Boolean): Option[LspContext[S]] =
      ctx.raw.pick(name, declared).map(rc => ctx.copy(raw = rc))

    override def pickHeader(ctx: LspContext[S]): LspContext[S] = ctx.copy(raw = ctx.raw.pickHeader)

    override def pickDeclared(ctx: LspContext[S]): LspContext[S] =
      ctx.copy(raw = ctx.raw.pickDeclared)
  }

  /*
    NOTE: This instance is used to generate LocationsAlgebra[S, State[LspContext[S], *]]
          to reuse the code from the body semantics in the header semantics
   */
  given [S[_]]: Lens[LspContext[S], LocationsState[S]] = {
    val get: LspContext[S] => LocationsState[S] =
      ctx => LocationsState(ctx.variables)
    val replace: LocationsState[S] => LspContext[S] => LspContext[S] =
      locs => ctx => ctx.copy(variables = locs.variables)

    Lens(get)(replace)
  }
}
