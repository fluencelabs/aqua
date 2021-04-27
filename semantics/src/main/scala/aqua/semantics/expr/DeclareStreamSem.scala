package aqua.semantics.expr

import aqua.model.Model
import aqua.parser.expr.DeclareStreamExpr
import aqua.semantics.Prog
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrayType, StreamType}
import cats.free.Free

class DeclareStreamSem[F[_]](val expr: DeclareStreamExpr[F]) {

  def program[Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    Prog.leaf(
      T.resolveType(expr.`type`)
        .flatMap {
          case Some(t: StreamType) =>
            N.define(expr.name, t)
          case Some(at @ ArrayType(t)) =>
            T.ensureTypeMatches(expr.`type`, StreamType(t), at)
          case Some(t) =>
            T.ensureTypeMatches(expr.`type`, StreamType(t), t)
          case None =>
            Free.pure[Alg, Boolean](false)
        }
        .map {
          case true => Model.empty(s"Name `${expr.name.value}` defined successfully")
          case false => Model.error(s"Name `${expr.name.value}` not defined")
        }
    )

}
