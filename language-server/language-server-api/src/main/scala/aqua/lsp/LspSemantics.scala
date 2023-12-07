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

  private def getImportTokens(ast: Ast[S]): List[LiteralToken[S]] =
    ast.collectHead {
      case ImportExpr(fn) => fn
      case ImportFromExpr(_, fn) => fn
      case UseExpr(fn, _) => fn
      case UseFromExpr(_, fn, _) => fn
    }.value.toList

  /**
   * Process the AST and return the semantics result.
   * NOTE: LspSemantics never return errors or warnings,
   * they are collected in LspContext.
   */
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
        variables = rawState.locations.variables ++ init.variables
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
          importTokens = importTokens,
          variables = state.locations.variables,
          errors = state.errors.toList,
          warnings = state.warnings.toList
        ).pure[Result]
      }
      // TODO: return as Eval
      .value
  }
}
