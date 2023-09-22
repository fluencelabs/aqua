package aqua.semantics

import aqua.parser.Ast
import aqua.semantics.SemanticError

import cats.data.{Chain, EitherNec, EitherT, NonEmptyChain, ValidatedNec, Writer}

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
}
