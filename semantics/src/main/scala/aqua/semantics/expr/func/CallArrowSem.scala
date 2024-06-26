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

import aqua.parser.expr.func.CallArrowExpr
import aqua.raw.Raw
import aqua.raw.ops.{Call, CallArrowRawTag, FuncOp}
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ProductType, StreamMapType, StreamType, Type}

import cats.Monad
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*

class CallArrowSem[S[_]](val expr: CallArrowExpr[S]) extends AnyVal {

  import expr.*

  private def getExports[Alg[_]: Monad](codomain: ProductType)(using
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg]
  ): Alg[List[Call.Export]] =
    variables.traverse(v => N.read(v, mustBeDefined = false).map(v -> _)).flatMap {
      case (v, Some(map @ StreamMapType(_))) :: Nil =>
        T.ensureTypeMatches(v, map.elementProduct, codomain)
          .as(Call.Export(v.value, map, isExistingStream = true) :: Nil)
      case vars =>
        (vars zip codomain.toList).traverse { case ((v, vType), t) =>
          vType match {
            case Some(stream @ StreamType(st)) =>
              T.ensureTypeMatches(v, st, t)
                .as(Call.Export(v.value, stream, isExistingStream = true))
            case _ =>
              N.define(v, t).as(Call.Export(v.value, t))
          }
        }
    }

  private def toModel[Alg[_]: Monad](using
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ): Alg[Option[FuncOp]] = for {
    // TODO: Accept other expressions
    callArrowRaw <- V.valueToCall(expr.callArrow)
    tag <- callArrowRaw.traverse { case (raw, at) =>
      getExports(at.codomain).flatMap(exports =>
        T.checkArrowCallResults(callArrow, at, variables, exports).as(CallArrowRawTag(exports, raw))
      )
    }
  } yield tag.map(_.funcOpLeaf)

  def program[Alg[_]: Monad](using
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    toModel[Alg].map(_.getOrElse(Raw.error("CallArrow can't be converted to Model")))

}
