package aqua.semantics.expr

import aqua.model.{ConstantModel, Model}
import aqua.parser.expr.ConstantExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.syntax.functor._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.Monad

class ConstantSem[S[_]](val expr: ConstantExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    V: ValuesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg]
  ): Prog[Alg, Model] = {
    for {
      defined <- N.constantDefined(expr.name)
      v <- V.valueToModel(expr.value)
      model <- (defined, v.map(v => v -> v.lastType), expr.skipIfAlreadyDefined) match {
        case (Some(definedType), Some((vm, actualType)), true) =>
          T.ensureTypeMatches(expr.value, definedType, actualType).map {
            case true =>
              Model.empty(s"Constant with name ${expr.name} was already defined, skipping")
            case false =>
              Model.error(s"Constant with name ${expr.name} was defined with different type")
          }
        case (Some(_), _, _) =>
          Model.error(s"Name '${expr.name.value}' was already defined").pure[Alg]
        case (_, None, _) =>
          Model.error(s"There is no such variable ${expr.value}").pure[Alg]
        case (_, Some(t), _) =>
          N.defineConstant(expr.name, t._2) as (ConstantModel(
            expr.name.value,
            t._1,
            expr.skipIfAlreadyDefined
          ): Model)
      }
    } yield model
  }
}
