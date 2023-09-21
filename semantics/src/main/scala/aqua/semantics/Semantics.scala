package aqua.semantics

import aqua.parser.Ast
import aqua.semantics.SemanticError

import cats.data.{Chain, EitherNec, EitherT, NonEmptyChain, ValidatedNec, Writer}

trait Semantics[S[_], C] {

  final type ProcessWarnings = [A] =>> Writer[
    Chain[SemanticWarning[S]],
    A
  ]

  final type ProcessResult = EitherT[
    ProcessWarnings,
    NonEmptyChain[SemanticError[S]],
    C
  ]

  def process(
    ast: Ast[S],
    init: C
  ): ProcessResult
}
