package aqua.semantics

import aqua.parser.Ast
import aqua.semantics.SemanticError

import cats.data.ValidatedNec

trait Semantics[S[_], C] {

  def process(
    ast: Ast[S],
    init: C
  ): ValidatedNec[SemanticError[S], C]
}
