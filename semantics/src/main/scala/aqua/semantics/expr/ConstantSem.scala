package aqua.semantics.expr

import aqua.parser.expr.ConstantExpr
import aqua.raw.{ConstantRaw, Raw}
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.Monad

class ConstantSem[S[_]](val expr: ConstantExpr[S]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
    V: ValuesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg]
  ): Prog[Alg, Raw] = {
    for {
      defined <- N.read(expr.name, false)
      v <- V.valueToRaw(expr.value)
      model <- (defined, v.map(v => v -> v.`type`), expr.skipIfAlreadyDefined) match {
        case (Some(definedType), Some((_, actualType)), true) =>
          T.ensureTypeMatches(expr.value, definedType, actualType).map {
            case true =>
              Raw.empty(s"Constant with name ${expr.name} was already defined, skipping")
            case false =>
              Raw.error(s"Constant with name ${expr.name} was defined with different type")
          }
        case (Some(_), _, _) =>
          Raw.error(s"Name '${expr.name.value}' was already defined").pure[Alg]
        case (_, None, _) =>
          Raw.error(s"There is no such variable ${expr.value}").pure[Alg]
        case (_, Some(t), _) =>
          N.defineRootValue(expr.name, t._2) as (ConstantRaw(
            expr.name.value,
            t._1,
            expr.skipIfAlreadyDefined
          ): Raw)
      }
    } yield model
  }
}
