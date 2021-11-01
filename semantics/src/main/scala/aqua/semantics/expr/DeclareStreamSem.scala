package aqua.semantics.expr

import aqua.model.func.raw.{DeclareStreamTag, FuncOp}
import aqua.model.{Model, VarModel}
import aqua.parser.expr.DeclareStreamExpr
import aqua.semantics.Prog
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrayType, OptionType, StreamType, Type}
import cats.data.Chain
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
            N.define(expr.name, t).map(b => Either.cond(b, t, ()))
          case Some(t: OptionType) =>
            val streamType = StreamType(t.element)
            N.define(expr.name, streamType).map(b => Either.cond(b, streamType, ()))
          case Some(at @ ArrayType(t)) =>
            val streamType = StreamType(t)
            T.ensureTypeMatches(expr.`type`, streamType, at).map(b => Either.cond(b, streamType, ()))
          case Some(t) =>
            val streamType = StreamType(t)
            T.ensureTypeMatches(expr.`type`, streamType, t).map(b => Either.cond(b, streamType, ()))
          case None =>
            Free.pure[Alg, Either[Unit, Type]](Left[Unit, Type](()))
        }
        .map {
          case Right(streamType) =>
            val valueModel = VarModel(expr.name.value, streamType, Chain.empty)
            FuncOp.leaf(DeclareStreamTag(valueModel, expr.name.value)): Model
          case Left(_) => Model.error(s"Name `${expr.name.value}` not defined")
        }
    )

}
