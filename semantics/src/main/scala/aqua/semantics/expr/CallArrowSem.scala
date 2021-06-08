package aqua.semantics.expr

import aqua.model.func.Call
import aqua.model.func.body.{CallArrowTag, CallServiceTag, FuncOp}
import aqua.model.{Model, ValueModel}
import aqua.parser.expr.CallArrowExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrowType, ScalarType, StreamType, Type}
import cats.Traverse
import cats.free.Free
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._

class CallArrowSem[F[_]](val expr: CallArrowExpr[F]) extends AnyVal {

  import expr._

  private def freeUnit[Alg[_]]: Free[Alg, Unit] = Free.pure[Alg, Unit](())

  private def checkArgsRes[Alg[_]](
    at: ArrowType
  )(implicit
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg]
  ): Free[Alg, (List[ValueModel], Option[Type])] =
    V.checkArguments(expr.funcName, at, args) >> variable
      .fold(freeUnit[Alg].as(Option.empty[Type]))(exportVar =>
        at.res.fold(
          // TODO: error! we're trying to export variable, but function has no export type
          freeUnit[Alg].as(Option.empty[Type])
        )(resType =>
          N.read(exportVar, mustBeDefined = false).flatMap {
            case Some(t @ StreamType(st)) =>
              T.ensureTypeMatches(exportVar, st, resType).as(Option(t))
            case _ => N.define(exportVar, resType).as(at.res)
          }
        )
      ) >>= { (v: Option[Type]) =>
      Traverse[List].traverse(args)(V.valueToModel).map(_.flatten).map(_ -> v)
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
        }.flatMap(_.fold(Free.pure[Alg, Option[FuncOp]](None)) { case (arrowType, serviceId) =>
          checkArgsRes(arrowType).map {
            case (argsResolved, t) =>
              Option(
                FuncOp.leaf(
                  CallServiceTag(
                    serviceId = serviceId,
                    funcName = funcName.value,
                    Call(argsResolved, (variable.map(_.value), t).mapN(Call.Export))
                  )
                )
              )

            case _ => None

          }
        })
      case None =>
        N.readArrow(funcName)
          .flatMap(_.fold(Free.pure[Alg, Option[FuncOp]](None)) { arrowType =>
            checkArgsRes(arrowType).map { case (argsResolved, t) =>
              FuncOp.leaf(
                CallArrowTag(
                  funcName = funcName.value,
                  Call(argsResolved, (variable.map(_.value), t).mapN(Call.Export))
                )
              )

            }
              .map(Option(_))
          })
    }

  def program[Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    toModel[Alg].map(_.getOrElse(Model.error("CallArrow can't be converted to Model")))

}
