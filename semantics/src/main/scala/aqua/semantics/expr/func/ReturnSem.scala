package aqua.semantics.expr.func

import aqua.model.func.raw.{FuncOp, ReturnTag}
import aqua.model.Model
import aqua.parser.expr.func.ReturnExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.data.NonEmptyList
import cats.syntax.flatMap.*
import cats.Monad

class ReturnSem[S[_]](val expr: ReturnExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    V: ValuesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg]
  ): Prog[Alg, Model] =
    expr.values
      .traverse(v => V.valueToModel(v).map(_.map(v -> _)))
      .map(_.toList.flatten)
      .map(NonEmptyList.fromList)
      .flatMap {
        case Some(vals) =>
          T.checkArrowReturn(vals).map[Model] {
            case true => FuncOp.leaf(ReturnTag(vals.map(_._2)))
            case false => Model.error("Return types validation failed")
          }
        case None =>
          Model.error("Return types resolution failed").pure[Alg]
      }
}
