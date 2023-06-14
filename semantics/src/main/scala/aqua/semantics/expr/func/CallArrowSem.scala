package aqua.semantics.expr.func

import aqua.parser.expr.func.CallArrowExpr
import aqua.raw.Raw
import aqua.raw.ops.{Call, CallArrowRawTag, FuncOp}
import aqua.raw.value.ValueRaw
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrowType, StreamType, Type}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{Monad, Traverse}
import aqua.raw.value.CallArrowRaw

class CallArrowSem[S[_]](val expr: CallArrowExpr[S]) extends AnyVal {

  import expr.*

  private def getExports[Alg[_]: Monad](callArrow: CallArrowRaw)(implicit
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg]
  ): Alg[List[Call.Export]] =
    (variables zip callArrow.baseType.codomain.toList).traverse { case (v, t) =>
      N.read(v, mustBeDefined = false).flatMap {
        case Some(stream @ StreamType(st)) =>
          T.ensureTypeMatches(v, st, t).as(Call.Export(v.value, stream))
        case _ =>
          N.define(v, t).as(Call.Export(v.value, t))
      }
    }

  private def toModel[Alg[_]: Monad](implicit
    N: NamesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ): Alg[Option[FuncOp]] = for {
    callArrowRaw <- V.callArrowToRaw(callArrow)
    maybeOp <- callArrowRaw.traverse(car =>
      variables
        .drop(car.baseType.codomain.length)
        .headOption
        .fold(getExports(car))(
          T.expectNoExport(_).as(Nil)
        )
        .map(maybeExports => CallArrowRawTag(maybeExports, car).funcOpLeaf)
    )
  } yield maybeOp

  def program[Alg[_]: Monad](implicit
    N: NamesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    toModel[Alg].map(_.getOrElse(Raw.error("CallArrow can't be converted to Model")))

}
