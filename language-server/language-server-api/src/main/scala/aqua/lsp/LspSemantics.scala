package aqua.lsp

import aqua.parser.Ast
import aqua.parser.head.{ImportExpr, ImportFromExpr, UseExpr, UseFromExpr}
import aqua.parser.lexer.{LiteralToken, Token}
import aqua.semantics.rules.locations.LocationsState
import aqua.semantics.{CompilerState, RawSemantics, SemanticError, SemanticWarning, Semantics}

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.foldable.*
import cats.syntax.either.*
import cats.syntax.reducible.*
import cats.data.{NonEmptyChain, ValidatedNec}
import monocle.Lens
import monocle.macros.GenLens

class LspSemantics[S[_]] extends Semantics[S, LspContext[S]] {

  def getImportTokens(ast: Ast[S]): List[LiteralToken[S]] = {
    ast.head.foldLeft[List[LiteralToken[S]]](Nil) { case (l, header) =>
      header match {
        case ImportExpr(fn) => l :+ fn
        case ImportFromExpr(_, fn) => l :+ fn
        case UseExpr(fn, _) => l :+ fn
        case UseFromExpr(_, fn, _) => l :+ fn
        case _ => l
      }
    }
  }

  def process(
    ast: Ast[S],
    init: LspContext[S]
  ): ProcessResult = {

    val rawState = CompilerState.init[S](init.raw)

    val initState = rawState.copy(
      names = rawState.names.copy(
        rootArrows = rawState.names.rootArrows ++ init.rootArrows,
        constants = rawState.names.constants ++ init.constants
      ),
      abilities = rawState.abilities.copy(
        definitions = rawState.abilities.definitions ++ init.abDefinitions
      ),
      locations = rawState.locations.copy(
        tokens = rawState.locations.tokens ++ init.tokens
      )
    )

    val importTokens = getImportTokens(ast)

    given Lens[CompilerState[S], LocationsState[S]] =
      GenLens[CompilerState[S]](_.locations)

    given LocationsInterpreter[S, CompilerState[S]] =
      new LocationsInterpreter[S, CompilerState[S]]()

    RawSemantics
      .interpret(ast, initState, init.raw)
      .map { case (state, ctx) =>
        LspContext(
          raw = ctx,
          rootArrows = state.names.rootArrows,
          constants = state.names.constants,
          abDefinitions = state.abilities.definitions,
          locations = state.locations.allLocations,
          importTokens = importTokens,
          tokens = state.locations.tokens,
          errors = state.errors.toList
        ).asRight.toEitherT[ProcessWarnings]
      }
      // TODO: return as Eval
      .value
  }
}
