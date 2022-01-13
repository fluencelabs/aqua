package aqua.semantics.expr.func

import aqua.raw.ops.Call
import aqua.raw.ops.{CallArrowTag, CallServiceTag, FuncOp}
import aqua.raw.Raw
import aqua.parser.expr.func.CallArrowExpr
import aqua.raw.value.ValueRaw
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrowType, StreamType, Type}
import cats.{Monad, Traverse}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*

class CallArrowSem[S[_]](val expr: CallArrowExpr[S]) extends AnyVal {

  import expr.*

  private def algUnit[Alg[_]: Monad]: Alg[Unit] = ().pure[Alg]

  private def checkArgsRes[Alg[_]: Monad](
    at: ArrowType
  )(implicit
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ): Alg[(List[ValueRaw], List[Type])] =
    V.checkArguments(expr.funcName, at, args) >> variables
      .foldLeft(algUnit[Alg].as((List.empty[Type], at.codomain.toList)))((f, exportVar) =>
        f.flatMap {
          case (exports, Nil) =>
            algUnit[Alg].as(exports -> Nil)

          case (exports, resType :: codom) =>
            N.read(exportVar, mustBeDefined = false).flatMap {
              case Some(t @ StreamType(st)) =>
                T.ensureTypeMatches(exportVar, st, resType).as((t :: exports, codom))
              case _ =>
                N.define(exportVar, resType).as((resType :: exports, codom))
            }
        }
      )
      .map(_._1) >>= { (v: List[Type]) =>
      Traverse[List].traverse(args)(V.valueToRaw).map(_.flatten -> v.reverse)
    }

  private def toModel[Alg[_]: Monad](implicit
    N: NamesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ): Alg[Option[FuncOp]] =
    ability match {
      case Some(ab) =>
        (A.getArrow(ab, funcName), A.getServiceId(ab)).mapN {
          case (Some(at), Right(sid)) =>
            Some(callServiceTag(at, Option(sid)))
          case (Some(at), Left(true)) =>
            Some(callServiceTag(at, None))
          case _ => None
        }.flatMap(_.traverse(identity))
      case None =>
        N.readArrow(funcName)
          .flatMap(_.map { arrowType =>
            callServiceTag(arrowType, None)
          }.traverse(identity))
    }

  def callServiceTag[Alg[_]: Monad](arrowType: ArrowType, serviceId: Option[ValueRaw])(implicit
    N: NamesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ): Alg[FuncOp] = {
    checkArgsRes(arrowType).flatMap { (argsResolved, resTypes) =>
      variables
        .drop(arrowType.codomain.length)
        .headOption
        .fold(
          (variables zip resTypes).map { case (v, t) =>
            Call.Export(v.value, t)
          }.pure[Alg]
        )(T.expectNoExport(_).as(Nil))
        .map(maybeExport =>
          FuncOp.leaf(serviceId match {
            case Some(sid) =>
              CallServiceTag(
                serviceId = sid,
                funcName = funcName.value,
                Call(argsResolved, maybeExport)
              )
            case None =>
              CallArrowTag(
                funcName = ability.map(_.value + "." + funcName.value).getOrElse(funcName.value),
                Call(argsResolved, maybeExport)
              )
          })
        )
    }
  }

  def program[Alg[_]: Monad](implicit
    N: NamesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    toModel[Alg].map(_.getOrElse(Raw.error("CallArrow can't be converted to Model")))

}
