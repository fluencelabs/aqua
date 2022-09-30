package aqua.model.inline.raw

import aqua.model.{
  CallModel,
  CallServiceModel,
  CanonicalizeModel,
  FlattenModel,
  ForModel,
  FunctorModel,
  IntoFieldModel,
  IntoIndexModel,
  LiteralModel,
  MatchMismatchModel,
  NextModel,
  PropertyModel,
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
  ApplyFunctorRaw,
  ApplyPropertyRaw,
  CallArrowRaw,
  FunctorRaw,
  IntoFieldRaw,
  IntoIndexRaw,
  LiteralRaw,
  PropertyRaw,
  ValueRaw,
  VarRaw
}
import aqua.types.{ArrayType, CanonStreamType, ScalarType, StreamType}
import cats.data.{Chain, State}
import cats.syntax.monoid.*
import cats.instances.list.*

object ApplyPropertiesRawInliner extends RawInliner[ApplyPropertyRaw] {

  private[inline] def removeProperty[S: Mangler: Exports: Arrows](
    vm: ValueModel
  ): State[S, (ValueModel, Inline)] =
    vm match {
      case VarModel(nameM, btm, propertyM) if propertyM.nonEmpty =>
        for {
          nameMM <- Mangler[S].findAndForbidName(nameM)
        } yield VarModel(nameMM, vm.`type`, Chain.empty) -> Inline.preload(
          // TODO use smth more resilient to make VarRaw from a flattened VarModel
          nameMM -> ApplyPropertyRaw.fromChain(VarRaw(nameM, btm), propertyM.map(_.toRaw))
        )
      case _ =>
        State.pure(vm -> Inline.empty)
    }

  private[inline] def unfoldProperty[S: Mangler: Exports: Arrows](
    p: PropertyRaw
  ): State[S, (PropertyModel, Inline)] = // TODO property for collection
    p match {
      case IntoFieldRaw(field, t) =>
        State.pure(IntoFieldModel(field, t) -> Inline.empty)
      case IntoIndexRaw(vm: ApplyPropertyRaw, t) =>
        for {
          nn <- Mangler[S].findAndForbidName("ap-prop")
        } yield IntoIndexModel(nn, t) -> Inline.preload(nn -> vm)

      case IntoIndexRaw(vr: (VarRaw | CallArrowRaw), t) =>
        unfold(vr, propertiesAllowed = false).map {
          case (VarModel(name, _, _), inline) => IntoIndexModel(name, t) -> inline
          case (LiteralModel(v, _), inline) => IntoIndexModel(v, t) -> inline
        }

      case IntoIndexRaw(LiteralRaw(value, _), t) =>
        State.pure(IntoIndexModel(value, t) -> Inline.empty)
    }

  private def increment(v: ValueModel, result: VarModel) =
    CallServiceModel(
      LiteralModel("\"math\"", ScalarType.string),
      "add",
      CallModel(
        v :: LiteralModel.fromRaw(LiteralRaw.number(1)) :: Nil,
        CallModel.Export(result.name, result.`type`) :: Nil
      )
    ).leaf

  def unfoldRawWithPropertyModels[S: Mangler: Exports: Arrows](
    raw: ValueRaw,
    propertyModels: Chain[PropertyModel],
    propertyPrefix: Inline,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    Exports[S].exports.flatMap { exports =>
      unfold(raw, propertiesAllowed).flatMap {
        case (v: VarModel, prefix) =>
          ((v.`type`, propertyModels.headOption) match {
            // canonicalize stream
            case (st: StreamType, Some(idx @ IntoIndexModel(_, _))) =>
              for {
                uniqueResultName <- Mangler[S].findAndForbidName(v.name + "_result_canon")
                uniqueTestName <- Mangler[S].findAndForbidName(v.name + "_test")
              } yield {
                val varSTest = VarModel(uniqueTestName, st)
                val iter = VarModel("s", st.element)

                val iterCanon = VarModel(v.name + "_iter_canon", CanonStreamType(st.element))

                val resultCanon =
                  VarModel(uniqueResultName, CanonStreamType(st.element), propertyModels)

                val incrVar = VarModel("incr_idx", ScalarType.u32)

                val tree = RestrictionModel(varSTest.name, true).wrap(
                  ForModel(iter.name, v).wrap(
                    increment(idx.idxToValueModel, incrVar),
                    PushToStreamModel(
                      iter,
                      CallModel.Export(varSTest.name, varSTest.`type`)
                    ).leaf,
                    CanonicalizeModel(
                      varSTest,
                      CallModel.Export(iterCanon.name, iterCanon.`type`)
                    ).leaf,
                    XorModel.wrap(
                      MatchMismatchModel(
                        iterCanon
                          .copy(properties = Chain.one(FunctorModel("length", ScalarType.`u32`))),
                        incrVar,
                        true
                      ).leaf,
                      NextModel(iter.name).leaf
                    )
                  ),
                  CanonicalizeModel(
                    varSTest,
                    CallModel.Export(resultCanon.name, CanonStreamType(st.element))
                  ).leaf
                )

                resultCanon -> Inline.tree(tree)
              }

            case _ =>
              val vm = v.copy(properties = v.properties ++ propertyModels).resolveWith(exports)
              State.pure(vm -> Inline.empty)
          }).flatMap { case (genV, genInline) =>
            if (propertiesAllowed) State.pure(genV -> (prefix |+| propertyPrefix |+| genInline))
            else
              removeProperty(genV).map { case (vmm, mpp) =>
                vmm -> (prefix |+| mpp |+| propertyPrefix |+| genInline)
              }
          }

        case (v, prefix) =>
          // What does it mean actually? I've no ides
          State.pure((v, prefix |+| propertyPrefix))
      }
    }
  }

  private def unfoldProperties[S: Mangler: Exports: Arrows](
    raw: ValueRaw,
    properties: Chain[PropertyRaw],
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    properties
      .foldLeft[State[S, (Chain[PropertyModel], Inline, ValueRaw)]](
        State.pure((Chain.empty[PropertyModel], Inline.empty, raw))
      ) { case (pcm, p) =>
        pcm.flatMap { case (pc, m, r) =>
          unfoldProperty(p).map { case (pm, mm) =>
            (pc :+ pm, m |+| mm, r)
          }
        }
      }
      .flatMap { case (propertyModels, map, r) =>
        unfoldRawWithPropertyModels(r, propertyModels, map, propertiesAllowed)
      }
  }

  override def apply[S: Mangler: Exports: Arrows](
    apr: ApplyPropertyRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] =
    val (raw, properties) = apr.unwind

    val leftToFunctor = properties.takeWhile {
      case FunctorRaw(_, _) => false
      case _ => true
    }

    if (leftToFunctor.length == properties.length) {
      unfoldProperties(raw, properties, propertiesAllowed)
    } else {
      // split properties like this:
      // properties -- functor -- properties with functors
      // process properties, process functor in ApplyFunctorRawInliner
      // then process tail recursively
      (for {
        ur <- properties.dropWhile {
          case FunctorRaw(_, _) => false
          case _ => true
        }.uncons
        (functor: FunctorRaw, right) = ur
      } yield {
        (leftToFunctor, functor, right)
      }).map { case (left, functor, right) =>
        for {
          vmLeftInline <- unfoldProperties(raw, left, propertiesAllowed)
          (leftVM, leftInline) = vmLeftInline
          fRaw = ApplyFunctorRaw(leftVM.toRaw, functor)
          vmFunctorInline <- ApplyFunctorRawInliner(fRaw, false)
          (fVM, fInline) = vmFunctorInline
          vmRightInline <- unfold(ApplyPropertyRaw.fromChain(fVM.toRaw, right), propertiesAllowed)
          (vm, rightInline) = vmRightInline
        } yield {
          vm -> (leftInline |+| fInline |+| rightInline)
        }
      }.getOrElse(unfoldProperties(raw, properties, propertiesAllowed))
    }



}
