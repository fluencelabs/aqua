package aqua.semantics.expr.func

import aqua.parser.expr.func.JoinExpr
import aqua.raw.Raw
import aqua.raw.ops.{FuncOp, JoinTag}
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*

class JoinSem[S[_]](val expr: JoinExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    V: ValuesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    expr.values
      .traverse(V.valueToRaw)
      .map(_.toList.flatten.distinct)
      .map(NonEmptyList.fromList)
      .map {
        case Some(vals) =>
          JoinTag(vals).funcOpLeaf
        case None =>
          Raw.error("Join values resolution failed")
      }
}
