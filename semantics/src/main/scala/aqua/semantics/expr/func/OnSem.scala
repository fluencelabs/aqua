package aqua.semantics.expr.func

import aqua.model.func.raw.{FuncOp, OnTag}
import aqua.model.{Model, ValueModel}
import aqua.parser.expr.func.OnExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{BoxType, OptionType, ScalarType}
import cats.{Monad, Traverse}
import cats.data.Chain
import cats.free.Free
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class OnSem[F[_]](val expr: OnExpr[F]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    V: ValuesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    Prog.around(
      (
        V.ensureIsString(expr.peerId),
        Traverse[List]
          .traverse(expr.via)(v =>
            V.valueToModel(v).flatTap {
              case Some(vm) =>
                vm.lastType match {
                  case _: BoxType =>
                    T.ensureTypeMatches(v, OptionType(ScalarType.string), vm.lastType)
                  case _ =>
                    T.ensureTypeMatches(v, ScalarType.string, vm.lastType)
                }
              case None => false.pure[Alg]
            }
          )
          .map(_.flatten)
      ).mapN { case (_, viaVM) =>
        viaVM
      }
        <* A.beginScope(expr.peerId),
      (viaVM: List[ValueModel], ops: Model) =>
        A.endScope() >> (ops match {
          case op: FuncOp =>
            V.valueToModel(expr.peerId).map {
              case Some(om) =>
                FuncOp.wrap(
                  OnTag(
                    om,
                    Chain.fromSeq(viaVM)
                  ),
                  op
                )
              case _ =>
                Model.error("OnSem: Impossible error")
            }

          case m => Model.error("On body is not an op, it's " + m).pure[Alg]
        })
    )
}
