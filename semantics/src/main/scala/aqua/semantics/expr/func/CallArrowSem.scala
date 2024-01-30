package aqua.semantics.expr.func

import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.{CallArrowToken, IntoArrow, IntoField, PropertyToken, VarToken}
import aqua.raw.Raw
import aqua.raw.ops.{Call, CallArrowRawTag, FuncOp}
import aqua.raw.value.CallArrowRaw
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ProductType, StreamType, Type}

import cats.Monad
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.comonad.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*

class CallArrowSem[S[_]](val expr: CallArrowExpr[S]) extends AnyVal {

  import expr.*

  private def getExports[Alg[_]: Monad](codomain: ProductType)(using
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg]
  ): Alg[List[Call.Export]] =
    (variables zip codomain.toList).traverse { case (v, t) =>
      N.read(v, mustBeDefined = false).flatMap {
        case Some(stream @ StreamType(st)) =>
          T.ensureTypeMatches(v, st, t).as(Call.Export(v.value, stream, isExistingStream = true))
        case _ =>
          N.define(v, t).as(Call.Export(v.value, t))
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
      getExports(at.codomain).map(CallArrowRawTag(_, raw)) <*
        T.checkArrowCallResults(callArrow, at, variables)
    }
  } yield tag.map(_.funcOpLeaf)

  def program[Alg[_]: Monad](using
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    toModel[Alg].map(_.getOrElse(Raw.error("CallArrow can't be converted to Model")))

}
