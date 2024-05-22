package aqua.semantics.expr.func

import aqua.helpers.syntax.optiont.*
import aqua.parser.expr.func.PushToStreamExpr
import aqua.parser.lexer.Token
import aqua.raw.Raw
import aqua.raw.ops.{Call, PushToMapTag, PushToStreamTag}
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.*
import aqua.types.TopType
import cats.Monad
import cats.data.OptionT
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
  )(using T: TypesAlgebra[S, Alg]): Alg[Boolean] = (
    T.typeToStream(streamToken, stream),
    T.typeToCollectible(elementToken, element)
  ).merged.semiflatMap { case (st, et) =>
    T.ensureTypeMatches(elementToken, st.element, et)
  }.getOrElse(false)

  def program[Alg[_]: Monad](using
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    N.read(expr.stream).flatMap {
      case None => Raw.error("Cannot resolve stream type").pure[Alg]
      case Some(st) =>
        expr.value match {
          case Left((key, value)) =>
            (V.valueToRaw(key), V.valueToRaw(value)).flatMapN {
              case (Some(k), Some(vm)) =>
                ensureStreamElementMatches(
                  expr.token,
                  value,
                  st,
                  vm.`type`
                ).flatMap {
                  case false =>
                    Raw.error("Stream type and element type does not match").pure[Alg]
                  case true =>
                    T.ensureTypeMatches(
                      key,
                      ScalarType.string,
                      k.`type`
                    ).map {
                      case false =>
                        Raw.error("Key for map can be only a string")
                      case true =>
                        PushToMapTag(k, vm, Call.Export(expr.stream.value, st)).funcOpLeaf
                    }
                }

              case _ => Raw.error("Cannot resolve value").pure[Alg]
            }
          case Right(value) =>
            V.valueToRaw(value).flatMap {
              case Some(vm) =>
                ensureStreamElementMatches(
                  expr.token,
                  value,
                  st,
                  vm.`type`
                ).map {
                  case false =>
                    Raw.error("Stream type and element type does not match")
                  case true =>
                    PushToStreamTag(vm, Call.Export(expr.stream.value, st)).funcOpLeaf
                }

              case _ => Raw.error("Cannot resolve value").pure[Alg]
            }
        }
    }

}
