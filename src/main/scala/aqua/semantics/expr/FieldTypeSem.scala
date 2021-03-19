package aqua.semantics.expr

import aqua.generator.Gen
import aqua.parser.expr.FieldTypeExpr
import aqua.semantics.Prog
import aqua.semantics.rules.types.TypesAlgebra
import cats.syntax.functor._

class FieldTypeSem[F[_]](val expr: FieldTypeExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit T: TypesAlgebra[F, Alg]): Prog[Alg, Gen] =
    T.resolveType(expr.`type`).flatMap {
      case Some(t) => T.defineField(expr.name, t) as Gen.noop
      case None => Gen.error.lift
    }

}
