package aqua.semantics.expr

import aqua.model.Model
import aqua.model.func.body.{FuncOp, MatchMismatchTag, XorTag}
import aqua.parser.expr.IfExpr
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.semantics.Prog
import aqua.types.Type
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
                .map(m => Some(lt -> rt).filter(_ => m))
            case None =>
              Free.pure[Alg, Option[(Type, Type)]](None)
          }
        case None =>
          V.resolveType(expr.right).as[Option[(Type, Type)]](None)
      },
      (r: Option[(Type, Type)], ops: Model) =>
        r.fold(Free.pure[Alg, Model](Model.error("If expression errored in matching types"))) {
          case (lt, rt) =>
            ops match {
              case op: FuncOp =>
                Free.pure[Alg, Model](
                  FuncOp.wrap(
                    XorTag.LeftBiased,
                    FuncOp.wrap(
                      MatchMismatchTag(
                        ValuesAlgebra.valueToModel(expr.left, lt),
                        ValuesAlgebra.valueToModel(expr.right, rt),
                        expr.eqOp.value
                      ),
                      op
                    )
                  )
                )
              case _ => Free.pure[Alg, Model](Model.error("Wrong body of the if expression"))
            }
        }
    )
}
