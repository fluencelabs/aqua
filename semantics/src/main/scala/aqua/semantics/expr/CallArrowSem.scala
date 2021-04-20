package aqua.semantics.expr

import aqua.model.func.Call
import aqua.model.func.body.{CallFunctionTag, CallServiceTag, FuncOp}
import aqua.model.Model
import aqua.parser.expr.CallArrowExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.ArrowType
import cats.free.Free
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.apply._

class CallArrowSem[F[_]](val expr: CallArrowExpr[F]) extends AnyVal {

  import expr._

  private def freeUnit[Alg[_]]: Free[Alg, Unit] = Free.pure[Alg, Unit](())

  private def checkArgsRes[Alg[_]](
    at: ArrowType
  )(implicit
    N: NamesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg]
  ): Free[Alg, List[Call.Arg]] =
    V.checkArguments(expr.funcName, at, args) >> variable
      .fold(freeUnit[Alg])(exportVar =>
        at.res.fold(
          // TODO: error! we're trying to export variable, but function has no export type
          freeUnit[Alg]
        )(resType => N.define(exportVar, resType).void)
      ) >> args.foldLeft(Free.pure[Alg, List[Call.Arg]](Nil)) { case (acc, v) =>
      (acc, V.resolveType(v)).mapN((a, b) => a ++ b.map(Call.Arg(ValuesAlgebra.valueToModel(v), _)))
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
          checkArgsRes(arrowType)
            .map(argsResolved =>
              FuncOp.leaf(
                CallServiceTag(
                  serviceId = ValuesAlgebra.valueToModel(serviceId),
                  funcName = funcName.value,
                  Call(argsResolved, variable.map(_.value))
                )
              )
            )
            .map(Option(_))
        })
      case None =>
        N.readArrow(funcName)
          .flatMap(_.fold(Free.pure[Alg, Option[FuncOp]](None)) { arrowType =>
            checkArgsRes(arrowType)
              .map(argsResolved =>
                FuncOp.leaf(
                  CallFunctionTag(
                    funcName = funcName.value,
                    Call(argsResolved, variable.map(_.value))
                  )
                )
              )
              .map(Option(_))
          })
    }

  def program[Alg[_]](implicit
    N: NamesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    toModel[Alg].map(_.getOrElse(Model.error("Coalgebra can't be converted to Model")))

}
