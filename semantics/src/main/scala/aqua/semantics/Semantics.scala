package aqua.semantics

import aqua.parser.Ast
import aqua.semantics.SemanticError

import cats.data.{Chain, EitherNec, EitherT, NonEmptyChain, ValidatedNec, Writer}

trait Semantics[S[_], W, E, C] {

  final type ProcessResult = Semantics.ProcessResult[W, E, C]

  def process(
    ast: Ast[S],
    init: C
  ): ProcessResult
}

object Semantics {

  type ProcessResult[W, E, C] =
    EitherT[Writer[Chain[W], *], NonEmptyChain[E], C]
}
