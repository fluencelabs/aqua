package aqua.model.inline.raw

import aqua.model.{CallModel, CanonicalizeModel, IntoFieldModel, IntoIndexModel, LambdaModel, LiteralModel, ValueModel, VarModel, SeqModel}
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{ApplyLambdaRaw, CallArrowRaw, IntoFieldRaw, IntoIndexRaw, LambdaRaw, LiteralRaw, VarRaw}
import aqua.types.{CanonStreamType, StreamType}
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
            val (genV, genInline) = v.`type` match {
              // canonicalize stream
              case st: StreamType =>
                val canonName = v.name + "_canon"
                val canonType = CanonStreamType(st.element)
                val vCanon = VarModel(canonName, canonType, lambda = v.lambda ++ lambdaModel)
                val tree = SeqModel.wrap(
                  CanonicalizeModel(v, CallModel.Export(canonName, canonType)).leaf,
                )
                vCanon -> Inline.tree(tree)
              case _ =>
                val vm = v.copy(lambda = v.lambda ++ lambdaModel).resolveWith(exports)
                vm -> Inline.empty
            }

            if (lambdaAllowed) State.pure(genV -> (prefix |+| map |+| genInline))
            else
              removeLambda(genV).map { case (vmm, mpp) =>
                vmm -> (prefix |+| mpp |+| map |+| genInline)
              }

          case (v, prefix) =>
            // What does it mean actually? I've no ides
            State.pure((v, prefix |+| map))
        }

      }
  }
}
