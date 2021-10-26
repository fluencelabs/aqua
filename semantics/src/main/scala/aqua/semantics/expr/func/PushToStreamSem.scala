package aqua.semantics.expr

import aqua.model.ValueModel.varName
import aqua.model.func.Call
import aqua.model.func.raw.{ApTag, FuncOp, FuncOps}
import aqua.model.{LiteralModel, Model, VarModel}
import aqua.parser.expr.func.PushToStreamExpr
import aqua.parser.lexer.Token
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{StreamType, Type}
import cats.free.Free
import cats.syntax.apply.*

class PushToStreamSem[F[_]](val expr: PushToStreamExpr[F]) extends AnyVal {

  private def ensureStreamElementMatches[Alg[_]](
    streamToken: Token[F],
    elementToken: Token[F],
    streamOp: Option[Type],
    elementOp: Option[Type]
  )(implicit
    T: TypesAlgebra[F, Alg]
  ): Free[Alg, Boolean] =
    (streamOp, elementOp).mapN { case (stream, element) =>
      stream match {
        case StreamType(st) =>
          T.ensureTypeMatches(elementToken, st, element)
        case _ =>
          T.ensureTypeMatches(streamToken, StreamType(element), stream)
      }

    }.getOrElse(Free.pure[Alg, Boolean](false))

  def program[Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    V.valueToModel(expr.value).flatMap {
      case Some(vm) =>
        for {
          resolvedStreamTypeOp <- N.read(expr.stream)
          valueType <- V.resolveType(expr.value)
          ensure <- ensureStreamElementMatches(
            expr.token,
            expr.value,
            resolvedStreamTypeOp,
            valueType
          )
        } yield {
          if (ensure)
            resolvedStreamTypeOp
              .map(t => FuncOps.ap(vm, Call.Export(expr.stream.value, t)): Model)
              .getOrElse(Model.error("Cannot resolve stream type"))
          else
            Model.error("Stream and pushed element types are not matches")
        }

      case _ => Free.pure[Alg, Model](Model.error("Cannot resolve value"))
    }

}
