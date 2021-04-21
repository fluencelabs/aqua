package aqua.semantics.expr

import aqua.model.{ConstantModel, Model}
import aqua.parser.expr.ConstantExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.free.Free
import cats.syntax.functor._

class ConstantSem[F[_]](val expr: ConstantExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit
    V: ValuesAlgebra[F, Alg],
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg]
  ): Prog[Alg, Model] = {
    for {
      defined <- N.constantDefined(expr.name)
      t <- V.resolveType(expr.value)
      model <- (defined, t, expr.mark) match {
        case (Some(_), Some(_), true) =>
          Free.pure[Alg, Model](
            Model.empty("Constant with existed name and '?=' generates no model")
          )
        case (Some(_), _, _) =>
          Free.pure[Alg, Model](Model.error(s"Name '${expr.name.value}' was already defined"))
        case (_, None, _) =>
          Free.pure[Alg, Model](Model.error(s"There is no such variable ${expr.value}"))
        case (_, Some(t), _) =>
          N.defineConstant(expr.name, t) as (ConstantModel(
            expr.name.value,
            ValuesAlgebra.valueToModel(expr.value)
          ): Model)
      }
    } yield model
  }
}
