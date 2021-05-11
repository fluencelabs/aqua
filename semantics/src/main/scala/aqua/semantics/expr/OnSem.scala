package aqua.semantics.expr

import aqua.model.Model
import aqua.model.func.body.{FuncOp, OnTag, ParTag}
import aqua.parser.expr.OnExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.types.ScalarType
import cats.data.Chain
import cats.syntax.flatMap._
import cats.syntax.functor._

class OnSem[F[_]](val expr: OnExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit
    V: ValuesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
//    Prog.after[Alg, Model] {
//      case g: FuncOp => Free.pure[Alg, Model](FuncOp.wrap(ParTag, g))
//      case g => Free.pure[Alg, Model](g)
//    }
    Prog.around(
      expr.via.foldLeft(
        V.ensureIsString(expr.peerId)
      ) { case (acc, v) =>
        acc >> V.ensureIsString(v)
      }
        >> A.beginScope(expr.peerId),
      (_: Unit, ops: Model) =>
        A.endScope() as (ops match {
          case op: FuncOp =>
            val funcOp = FuncOp.wrap(
              OnTag(
                ValuesAlgebra.valueToModel(expr.peerId, ScalarType.string),
                Chain.fromSeq(expr.via).map(ValuesAlgebra.valueToModel(_, ScalarType.string))
              ),
              op
            )
            expr.parPrefix.fold(funcOp)(_ => FuncOp.wrap(ParTag, funcOp))
          case m => Model.error("On body is not an op, it's " + m)
        })
    )
}
