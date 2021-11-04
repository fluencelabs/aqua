package aqua.semantics.expr.func

import aqua.model.ValueModel.varName
import aqua.model.func.Call
import aqua.model.func.raw.{FuncOp, FuncOps, PushToStreamTag}
import aqua.model.{LiteralModel, Model, VarModel}
import aqua.parser.expr.func.PushToStreamExpr
import aqua.parser.lexer.Token
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrayType, StreamType, Type}
import cats.Monad
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class PushToStreamSem[S[_]](val expr: PushToStreamExpr[S]) extends AnyVal {

  private def ensureStreamElementMatches[Alg[_]: Monad](
    streamToken: Token[S],
    elementToken: Token[S],
    stream: Type,
    element: Type
  )(implicit
    T: TypesAlgebra[S, Alg]
  ): Alg[Boolean] =
    stream match {
      case StreamType(st) =>
        T.ensureTypeMatches(elementToken, st, element)
      case _ =>
        T.ensureTypeMatches(
          streamToken,
          StreamType(element match {
            case StreamType(e) => ArrayType(e)
            case _ => element
          }),
          stream
        )
    }

  def program[Alg[_]: Monad](implicit
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ): Prog[Alg, Model] =
    V.valueToModel(expr.value).flatMap {
      case Some(vm) =>
        N.read(expr.stream).flatMap {
          case None => Model.error("Cannot resolve stream type").pure[Alg]
          case Some(t) =>
            ensureStreamElementMatches(
              expr.token,
              expr.value,
              t,
              vm.lastType
            ).map {
              case false =>
                Model.error("Stream type and element type does not match")
              case true =>
                FuncOps.pushToStream(vm, Call.Export(expr.stream.value, t)): Model
            }
        }

      case _ => Model.error("Cannot resolve value").pure[Alg]
    }

}
