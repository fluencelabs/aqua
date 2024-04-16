package aqua.semantics

import aqua.parser.Ast

import cats.data.{Chain, EitherT, NonEmptyChain, Writer}
import cats.syntax.functor.*

trait Semantics[S[_], C] {

  final type Warnings = [A] =>> Writer[
    Chain[SemanticWarning[S]],
    A
  ]

  final type Result = [A] =>> EitherT[
    Warnings,
    NonEmptyChain[SemanticError[S]],
    A
  ]

  final type ProcessResult = Result[C]

  def process(
    ast: Ast[S],
    init: C
  ): ProcessResult

  def stateToResult(state: CompilerState[S], ctx: C): ProcessResult =
    EitherT(
      Writer
        .tell(state.warnings)
        .as(
          NonEmptyChain
            .fromChain(state.errors)
            .toLeft(ctx)
        )
    )
}
