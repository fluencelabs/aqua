package aqua.semantics.expr

import aqua.model.Model
import aqua.model.func.body.{FuncOp, MatchMismatchTag}
import aqua.parser.expr.IfExpr
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.semantics.Prog
import cats.free.Free
import cats.syntax.functor._

class IfSem[F[_]](val expr: IfExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit
    V: ValuesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    Prog.around(
      V.resolveType(expr.left).flatMap {
        case Some(lt) =>
          V.resolveType(expr.right).flatMap {
            case Some(rt) =>
              T.ensureTypeMatches(expr.right, lt, rt)
            case None =>
              Free.pure[Alg, Boolean](false)
          }
        case None =>
          V.resolveType(expr.right).as(false)
      },
      (r: Boolean, ops: Model) =>
        ops match {
          case op: FuncOp if r =>
            Free.pure[Alg, Model](
              FuncOp.wrap(
                MatchMismatchTag(
                  ValuesAlgebra.valueToModel(expr.left),
                  ValuesAlgebra.valueToModel(expr.right),
                  expr.eqOp.value
                ),
                op
              )
            )
          case _ => Free.pure[Alg, Model](Model.error("If expression errored"))
        }
    )
}
