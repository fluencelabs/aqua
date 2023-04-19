package aqua.lsp

import aqua.parser.Ast
import aqua.parser.head.{ImportExpr, ImportFromExpr}
import aqua.parser.lexer.LiteralToken
import aqua.semantics.{CompilerState, SemanticError, Semantics}
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.foldable.*
import cats.syntax.reducible.*
import cats.data.{NonEmptyChain, ValidatedNec}

class LspSemantics[S[_]] extends Semantics[S, LspContext[S]] {

  def getImportTokens(ast: Ast[S]): List[LiteralToken[S]] = {
    ast.head.foldLeft[List[LiteralToken[S]]](Nil){ case (l, header) =>
      header match {
        case ImportExpr(fn) =>
          println("import: " + fn)
          l :+ fn
        case ImportFromExpr(_, fn) => l :+ fn
        case _ => l
      }
    }
  }

  def process(
               ast: Ast[S],
               init: LspContext[S]
             ): ValidatedNec[SemanticError[S], LspContext[S]] = {

    val rawState = CompilerState.init[S](init.raw)
    val initState = rawState.copy(
      names = rawState.names.copy(
        rootArrows = rawState.names.rootArrows ++ init.rootArrows,
        constants = rawState.names.constants ++ init.constants
      ),
      abilities = rawState.abilities.copy(
        definitions = rawState.abilities.definitions ++ init.abDefinitions
      )
    )

    val importTokens = getImportTokens(ast)


    Semantics
      .interpret(ast, initState, init.raw)
      .map { case (state, ctx) =>
        NonEmptyChain
          .fromChain(state.errors)
          .fold[ValidatedNec[SemanticError[S], LspContext[S]]] {
            Valid(
              LspContext(
                raw = ctx,
                rootArrows = state.names.rootArrows,
                constants = state.names.constants,
                abDefinitions = state.abilities.definitions,
                locations = state.locations,
                importTokens = importTokens
              )
            )
          }(Invalid(_))
      }
      // TODO: return as Eval
      .value
  }
}