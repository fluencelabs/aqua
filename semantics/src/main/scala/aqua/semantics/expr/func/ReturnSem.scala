package aqua.semantics.expr.func

import aqua.model.func.raw.{FuncOp, ReturnTag}
import aqua.model.Model
import aqua.parser.expr.func.ReturnExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.data.NonEmptyList
import cats.syntax.flatMap.*
import cats.Monad

class ReturnSem[S[_]](val expr: ReturnExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit V: ValuesAlgebra[S, Alg]): Prog[Alg, Model] =
    expr.values
      .traverse(V.valueToModel)
      .map(_.toList.flatten)
      // TODO: check if it should be reversed or not
      // TODO: maybe we need to get expected return types from the context and check there
      .map(NonEmptyList.fromList)
      .map[Model] {
        case Some(vals) => FuncOp.leaf(ReturnTag(vals))
        case None => Model.error("Cannot resolve return types")
      }
}
