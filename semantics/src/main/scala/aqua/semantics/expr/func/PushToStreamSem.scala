/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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
import cats.syntax.option.*
import cats.syntax.traverse.*

class PushToStreamSem[S[_]](val expr: PushToStreamExpr[S]) extends AnyVal {

  private def ensureStreamElementMatches[Alg[_]: Monad](
    streamToken: Token[S],
    elementToken: Token[S],
    stream: Type,
    element: Type,
    isMap: Boolean
  )(using T: TypesAlgebra[S, Alg]): Alg[Boolean] = (
    (if (isMap) T.typeToStreamMap(streamToken, stream) else T.typeToStream(streamToken, stream))
      .map(s => s: MutableStreamType),
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
        ensureStreamElementMatches(expr.token, value, st, valueRaw.`type`, key.isDefined)
      )
    } yield (keyRaw match {
      case Some(k) => PushToMapTag(k, valueRaw, Call.Export(expr.stream.value, st))
      case None => PushToStreamTag(valueRaw, Call.Export(expr.stream.value, st))
    }).funcOpLeaf).getOrElse(Raw.error("Cannot resolve push to stream"))

}
