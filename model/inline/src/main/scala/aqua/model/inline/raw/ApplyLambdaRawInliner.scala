package aqua.model.inline.raw

import aqua.model.{IntoFieldModel, IntoIndexModel, LambdaModel, LiteralModel, ValueModel, VarModel}
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{
  ApplyLambdaRaw,
  CallArrowRaw,
  IntoFieldRaw,
  IntoIndexRaw,
  LambdaRaw,
  LiteralRaw,
  VarRaw
}
import cats.data.{Chain, State}
import cats.syntax.monoid.*
import cats.instances.list.*

object ApplyLambdaRawInliner extends RawInliner[ApplyLambdaRaw] {

  private[inline] def removeLambda[S: Mangler: Exports: Arrows](
    vm: ValueModel
  ): State[S, (ValueModel, Inline)] =
    vm match {
      case VarModel(nameM, btm, lambdaM) if lambdaM.nonEmpty =>
        for {
          nameMM <- Mangler[S].findAndForbidName(nameM)
        } yield VarModel(nameMM, vm.`type`, Chain.empty) -> Inline.preload(
          // TODO use smth more resilient to make VarRaw from a flattened VarModel
          nameMM -> ApplyLambdaRaw.fromChain(VarRaw(nameM, btm), lambdaM.map(_.toRaw))
        )
      case _ =>
        State.pure(vm -> Inline.empty)
    }

  private[inline] def unfoldLambda[S: Mangler: Exports: Arrows](
    l: LambdaRaw
  ): State[S, (LambdaModel, Inline)] = // TODO lambda for collection
    l match {
      case IntoFieldRaw(field, t) => State.pure(IntoFieldModel(field, t) -> Inline.empty)
      case IntoIndexRaw(vm: ApplyLambdaRaw, t) =>
        for {
          nn <- Mangler[S].findAndForbidName("ap-lambda")
        } yield IntoIndexModel(nn, t) -> Inline.preload(nn -> vm)

      case IntoIndexRaw(vr: (VarRaw | CallArrowRaw), t) =>
        unfold(vr, lambdaAllowed = false).map {
          case (VarModel(name, _, _), inline) => IntoIndexModel(name, t) -> inline
          case (LiteralModel(v, _), inline) => IntoIndexModel(v, t) -> inline
        }

      case IntoIndexRaw(LiteralRaw(value, _), t) =>
        State.pure(IntoIndexModel(value, t) -> Inline.empty)
    }

  override def apply[S: Mangler: Exports: Arrows](
    alr: ApplyLambdaRaw,
    lambdaAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = Exports[S].exports.flatMap { exports =>
    val (raw, lambda) = alr.unwind
    lambda
      .foldLeft[State[S, (Chain[LambdaModel], Inline)]](
        State.pure(Chain.empty[LambdaModel] -> Inline.empty)
      ) { case (lcm, l) =>
        lcm.flatMap { case (lc, m) =>
          unfoldLambda(l).map { case (lm, mm) =>
            (lc :+ lm, m |+| mm)
          }
        }
      }
      .flatMap { case (lambdaModel, map) =>
        unfold(raw, lambdaAllowed).flatMap {
          case (v: VarModel, prefix) =>
            val vm = v.copy(lambda = v.lambda ++ lambdaModel).resolveWith(exports)
            if (lambdaAllowed) State.pure(vm -> (prefix |+| map))
            else
              removeLambda(vm).map { case (vmm, mpp) =>
                vmm -> (prefix |+| mpp |+| map)
              }
          case (v, prefix) =>
            // What does it mean actually? I've no ides
            State.pure((v, prefix |+| map))
        }

      }
  }
}
