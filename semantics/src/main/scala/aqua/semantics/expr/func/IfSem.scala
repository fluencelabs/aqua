package aqua.semantics.expr

import aqua.model.{Model, ValueModel}
import aqua.model.func.raw.{FuncOp, MatchMismatchTag, XorTag}
import aqua.parser.expr.IfExpr
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.semantics.Prog
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.types.Type
import cats.syntax.functor._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.Monad

class IfSem[F[_]](val expr: IfExpr[F]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    V: ValuesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    Prog
      .around(
        V.valueToModel(expr.left).flatMap {
          case Some(lt) =>
            V.valueToModel(expr.right).flatMap {
              case Some(rt) =>
                T.ensureTypeMatches(expr.right, lt.lastType, rt.lastType)
                  .map(m => Some(lt -> rt).filter(_ => m))
              case None =>
                None.pure[Alg]
            }
          case None =>
            V.resolveType(expr.right).as[Option[(ValueModel, ValueModel)]](None)
        },
        (r: Option[(ValueModel, ValueModel)], ops: Model) =>
          r.fold(Model.error("If expression errored in matching types").pure[Alg]) {
            case (lt, rt) =>
              ops match {
                case op: FuncOp =>
                  FuncOp
                    .wrap(
                      XorTag.LeftBiased,
                      FuncOp.wrap(
                        MatchMismatchTag(
                          lt,
                          rt,
                          expr.eqOp.value
                        ),
                        op
                      )
                    )
                    .pure[Alg]

                case _ => Model.error("Wrong body of the if expression").pure[Alg]
              }
          }
      )
      .abilitiesScope(expr.token)
}
