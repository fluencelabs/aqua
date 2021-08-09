package aqua.semantics.expr

import aqua.model.func.Call
import aqua.model.func.raw.{CallArrowTag, CallServiceTag, FuncOp}
import aqua.model.{Model, ValueModel}
import aqua.parser.expr.CallArrowExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrowType, StreamType, Type}
import cats.Traverse
import cats.free.Free
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._

class CallArrowSem[F[_]](val expr: CallArrowExpr[F]) extends AnyVal {

  import expr._

  private def freeUnit[Alg[_]]: Free[Alg, Unit] = Free.pure[Alg, Unit](())

  private def checkArgsRes[Alg[_]](
    at: ArrowType
  )(implicit
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg]
  ): Free[Alg, (List[ValueModel], List[Type])] =
    V.checkArguments(expr.funcName, at, args) >> variables
      .foldLeft(freeUnit[Alg].as((List.empty[Type], at.codomain.toList)))((f, exportVar) =>
        f.flatMap {
          case (exports, Nil) =>
            freeUnit[Alg].as(exports -> Nil)

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
      Traverse[List].traverse(args)(V.valueToModel).map(_.flatten -> v.reverse)
    }

  private def toModel[Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg]
  ): Free[Alg, Option[FuncOp]] =
    ability match {
      case Some(ab) =>
        (A.getArrow(ab, funcName), A.getServiceId(ab)).mapN {
          case (Some(at), Some(sid)) =>
            Option(at -> sid) // Here we assume that Ability is a Service that must be resolved
          case _ => None
        }.flatMap(_.map { case (arrowType, serviceId) =>
          callServiceTag(arrowType, Option(serviceId))
        }.traverse(identity))
      case None =>
        N.readArrow(funcName)
          .flatMap(_.map { arrowType =>
            callServiceTag(arrowType, None)
          }.traverse(identity))
    }

  def callServiceTag[Alg[_]](arrowType: ArrowType, serviceId: Option[ValueModel])(implicit
    N: NamesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg]
  ): Free[Alg, FuncOp] = {
    checkArgsRes(arrowType).flatMap { (argsResolved, resTypes) =>
      variables
        .drop(arrowType.codomain.length)
        .headOption
        .fold(
          Free.pure((variables zip resTypes).map { case (v, t) =>
            Call.Export(v.value, t)
          })
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
                funcName = funcName.value,
                Call(argsResolved, maybeExport)
              )
          })
        )
    }
  }

  def program[Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    toModel[Alg].map(_.getOrElse(Model.error("CallArrow can't be converted to Model")))

}
