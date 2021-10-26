package aqua.semantics.expr

import aqua.model.ValueModel.varName
import aqua.model.func.Call
import aqua.model.func.raw.{ApTag, FuncOp, FuncOps}
import aqua.model.{LiteralModel, Model, VarModel}
import aqua.parser.expr.PushToStreamExpr
import aqua.parser.lexer.Token
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrayType, StreamType, Type}
import cats.free.Free
import cats.syntax.apply.*

class PushToStreamSem[F[_]](val expr: PushToStreamExpr[F]) extends AnyVal {

  private def ensureStreamElementMatches[Alg[_]](
    streamToken: Token[F],
    elementToken: Token[F],
    stream: Type,
    element: Type
  )(implicit
    T: TypesAlgebra[F, Alg]
  ): Free[Alg, Boolean] =
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

  def program[Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    V.valueToModel(expr.value).flatMap {
      case Some(vm) =>
        N.read(expr.stream).flatMap {
          case None => Free.pure[Alg, Model](Model.error("Cannot resolve stream type"))
          case Some(t) =>
            ensureStreamElementMatches(
              expr.token,
              expr.value,
              t,
              vm.lastType
            ).flatMap {
              case false =>
                Free.pure[Alg, Model](Model.error("Stream type and element type does not match"))
              case true =>
                vm.lastType match {
                  case StreamType(lt) =>
                    // https://github.com/fluencelabs/aqua/issues/277
                    // TODO: get Name from value for the opaque thing
                    N.defineOpaque(expr.stream, ArrayType(lt)).map { n =>
                      val opaqueVar = VarModel(n.value, ArrayType(lt))
                      FuncOps.seq(
                        FuncOps.can(vm, Call.Export(opaqueVar.name, opaqueVar.lastType)),
                        FuncOps.ap(opaqueVar, Call.Export(expr.stream.value, t))
                      ): Model
                    }

                  case _ =>
                    Free.pure[Alg, Model](FuncOps.ap(vm, Call.Export(expr.stream.value, t)): Model)
                }
            }
        }

      case _ => Free.pure[Alg, Model](Model.error("Cannot resolve value"))
    }

}
