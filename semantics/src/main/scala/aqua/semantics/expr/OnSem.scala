package aqua.semantics.expr

import aqua.model.func.raw.{FuncOp, OnTag}
import aqua.model.{Model, ValueModel}
import aqua.parser.expr.OnExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{BoxType, OptionType, ScalarType}
import cats.Traverse
import cats.data.Chain
import cats.free.Free
import cats.syntax.apply._
import cats.syntax.flatMap._

class OnSem[F[_]](val expr: OnExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit
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
              case None => Free.pure(false)
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

          case m => Free.pure[Alg, Model](Model.error("On body is not an op, it's " + m))
        })
    )
}
