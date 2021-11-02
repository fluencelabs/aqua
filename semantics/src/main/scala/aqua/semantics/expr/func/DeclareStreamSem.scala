package aqua.semantics.expr.func

import aqua.model.Model
import aqua.parser.expr.func.DeclareStreamExpr
import aqua.semantics.Prog
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrayType, OptionType, StreamType}
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.Monad

class DeclareStreamSem[F[_]](val expr: DeclareStreamExpr[F]) {

  def program[Alg[_]: Monad](implicit
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    Prog.leaf(
      T.resolveType(expr.`type`)
        .flatMap {
          case Some(t: StreamType) =>
            N.define(expr.name, t).map(b => Option.when(b)(t))
          case Some(t: OptionType) =>
            val streamType = StreamType(t.element)
            N.define(expr.name, streamType).map(b => Option.when(b)(streamType))
          case Some(at @ ArrayType(t)) =>
            val streamType = StreamType(t)
            T.ensureTypeMatches(expr.`type`, streamType, at).map(b => Option.when(b)(streamType))
          case Some(t) =>
            val streamType = StreamType(t)
            T.ensureTypeMatches(expr.`type`, streamType, t).map(b => Option.when(b)(streamType))
          case None =>
            None.pure[Alg]
        }
        .map {
          case Some(streamType) =>
            val valueModel = VarModel(expr.name.value, streamType, Chain.empty)
            FuncOp.leaf(DeclareStreamTag(valueModel)): Model
          case None => Model.error(s"Name `${expr.name.value}` not defined")
        }
    )

}
