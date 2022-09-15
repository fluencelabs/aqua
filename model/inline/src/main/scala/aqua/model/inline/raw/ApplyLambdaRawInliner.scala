package aqua.model.inline.raw

import aqua.model.{
  CallModel,
  CallServiceModel,
  CanonicalizeModel,
  FlattenModel,
  ForModel,
  IntoFieldModel,
  IntoIndexModel,
  LambdaModel,
  LiteralModel,
  MatchMismatchModel,
  NextModel,
  PushToStreamModel,
  RestrictionModel,
  SeqModel,
  ValueModel,
  VarModel,
  XorModel
}
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
import aqua.types.{ArrayType, CanonStreamType, ScalarType, StreamType}
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
    l: LambdaRaw,
    canonicalizeStream: Boolean
  ): State[S, (LambdaModel, Inline)] = // TODO lambda for collection
    l match {
      case IntoFieldRaw(field, t) => State.pure(IntoFieldModel(field, t) -> Inline.empty)
      case IntoIndexRaw(vm: ApplyLambdaRaw, t) =>
        for {
          nn <- Mangler[S].findAndForbidName("ap-lambda")
        } yield IntoIndexModel(nn, t) -> Inline.preload(nn -> vm)

      case IntoIndexRaw(vr: (VarRaw | CallArrowRaw), t) =>
        unfold(vr, lambdaAllowed = false, canonicalizeStream = canonicalizeStream).map {
          case (VarModel(name, _, _), inline) => IntoIndexModel(name, t) -> inline
          case (LiteralModel(v, _), inline) => IntoIndexModel(v, t) -> inline
        }

      case IntoIndexRaw(LiteralRaw(value, _), t) =>
        State.pure(IntoIndexModel(value, t) -> Inline.empty)
    }

  private def getLength(v: ValueModel, result: VarModel) =
    CallServiceModel(
      LiteralModel("\"op\"", ScalarType.string),
      "array_length",
      CallModel(v :: Nil, CallModel.Export(result.name, result.`type`) :: Nil)
    ).leaf

  private def increment(v: ValueModel, result: VarModel) =
    CallServiceModel(
      LiteralModel("\"math\"", ScalarType.string),
      "add",
      CallModel(
        v :: LiteralModel.fromRaw(LiteralRaw.number(1)) :: Nil,
        CallModel.Export(result.name, result.`type`) :: Nil
      )
    ).leaf

  override def apply[S: Mangler: Exports: Arrows](
    alr: ApplyLambdaRaw,
    lambdaAllowed: Boolean,
    canonicalizeStream: Boolean
  ): State[S, (ValueModel, Inline)] = Exports[S].exports.flatMap { exports =>
    val (raw, lambda) = alr.unwind
    lambda
      .foldLeft[State[S, (Chain[LambdaModel], Inline)]](
        State.pure(Chain.empty[LambdaModel] -> Inline.empty)
      ) { case (lcm, l) =>
        lcm.flatMap { case (lc, m) =>
          unfoldLambda(l, canonicalizeStream).map { case (lm, mm) =>
            (lc :+ lm, m |+| mm)
          }
        }
      }
      .flatMap { case (lambdaModel, map) =>
        unfold(raw, lambdaAllowed, canonicalizeStream).flatMap {
          case (v: VarModel, prefix) =>
            val (genV, genInline) = (v.`type`, lambdaModel.headOption) match {
              // canonicalize stream
              case (st: StreamType, Some(idx @ IntoIndexModel(_, _))) if canonicalizeStream =>
                val varSTest = VarModel(v.name + "_test", st)
                val iter = VarModel("s", st.element)

                val iterCanon = VarModel(v.name + "_iter_canon", CanonStreamType(st.element))

                // TODO: get unique name
                val resultCanon = VarModel(v.name + "_result_canon", CanonStreamType(st.element), lambdaModel)

                val lengthVar = VarModel("s_length", ScalarType.i64)
                val incrVar = VarModel("incr_idx", ScalarType.i64)

                val tree = RestrictionModel(varSTest.name, true).wrap(
                  ForModel(iter.name, v).wrap(
                    increment(idx.idxToValueModel, incrVar),
                    PushToStreamModel(iter, CallModel.Export(varSTest.name, varSTest.`type`)).leaf,
                    CanonicalizeModel(
                      varSTest,
                      CallModel.Export(iterCanon.name, iterCanon.`type`)
                    ).leaf,
                    XorModel.wrap(
                      SeqModel.wrap(
                        getLength(iterCanon, lengthVar),
                        MatchMismatchModel(lengthVar, incrVar, true).leaf
                      ),
                      NextModel(iter.name).leaf
                    )
                  ),
                  CanonicalizeModel(
                    varSTest,
                    CallModel.Export(resultCanon.name, CanonStreamType(st.element))
                  ).leaf
                )

                resultCanon -> Inline.tree(tree)
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
