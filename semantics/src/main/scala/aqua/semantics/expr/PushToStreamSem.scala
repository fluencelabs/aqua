package aqua.semantics.expr

import aqua.model.func.Call
import aqua.model.func.raw.{CallServiceTag, FuncOp}
import aqua.model.{LiteralModel, Model}
import aqua.parser.expr.PushToStreamExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.free.Free

class PushToStreamSem[F[_]](val expr: PushToStreamExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    V.valueToModel(expr.value).flatMap {
      case Some(vm) =>
        for {
          resolvedStreamType <- N.read(expr.stream.name).map(_.get)
          valueType <- V.resolveType(expr.value)
          _ <- T.expectStream(expr.stream, resolvedStreamType)
          _ <- T.expectStreamMember(expr.value, resolvedStreamType, valueType)
        } yield {
          FuncOp
            .leaf(
              CallServiceTag(
                LiteralModel.quote("op"),
                "identity",
                Call(vm :: Nil, Some(Call.Export(expr.stream.name.value, resolvedStreamType)))
              )
            ): Model
        }

      case _ => Free.pure[Alg, Model](Model.error("Cannot resolve stream type"))
    }

}
