package aqua.semantics.expr.func

import aqua.raw.ops.ReturnTag
import aqua.parser.expr.func.ReturnExpr
import aqua.raw.Raw
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
  ): Prog[Alg, Raw] =
    expr.values
      .traverse(v => V.valueToRaw(v).map(_.map(v -> _)))
      .map(_.toList.flatten)
      .map(NonEmptyList.fromList)
      .flatMap {
        case Some(vals) =>
          T.checkArrowReturn(vals).map[Raw] {
            case true => ReturnTag(vals.map(_._2)).leaf.toFuncOp
            case false => Raw.error("Return types validation failed")
          }
        case None =>
          Raw.error("Return types resolution failed").pure[Alg]
      }
}
