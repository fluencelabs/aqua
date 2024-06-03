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
import cats.syntax.traverse.*
import cats.syntax.functor.*
import cats.syntax.option.*

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
    (for {
      st <- OptionT(N.read(expr.stream))
      (key, value) = expr.value.fold(
        { case (key, value) => (key.some, value) },
        value => (none, value)
      )
      keyRaw <- key.traverse(k =>
        for {
          raw <- OptionT(V.valueToRaw(k))
          _ <- OptionT.withFilterF(
            T.ensureTypeMatches(k, ScalarType.string, raw.`type`)
          )
        } yield raw
      )
      valueRaw <- OptionT(V.valueToRaw(value))
      _ <- OptionT.withFilterF(
        ensureStreamElementMatches(expr.token, value, st, valueRaw.`type`)
      )
    } yield (keyRaw match {
      case Some(k) => PushToMapTag(k, valueRaw, Call.Export(expr.stream.value, st))
      case None => PushToStreamTag(valueRaw, Call.Export(expr.stream.value, st))
    }).funcOpLeaf).getOrElse(Raw.error("Cannot resolve push to stream"))

}
