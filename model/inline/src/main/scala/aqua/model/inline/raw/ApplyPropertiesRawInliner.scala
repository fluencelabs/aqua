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
  ApplyGateRaw,
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
import aqua.types.{ArrayType, CanonStreamType, ScalarType, StreamType, Type}
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

  /**
   * @param valueWithFirstProperty pass value only with the first property to check if we are trying to join a stream
   */
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

  def reachModelWithPropertyModels[S: Mangler: Exports: Arrows](
    model: ValueModel,
    propertyModels: Chain[PropertyModel],
    propertyPrefix: Inline,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    Exports[S].exports.flatMap { exports =>
      model match {
        case v: VarModel =>
          {
            val vm = v.copy(properties = v.properties ++ propertyModels).resolveWith(exports)
            State.pure((vm, Inline.empty))
          }.flatMap { case (genV, genInline) =>
            val prefInline = propertyPrefix |+| genInline
            if (propertiesAllowed) State.pure(genV -> prefInline)
            else
              removeProperty(genV).map { case (vmm, mpp) =>
                vmm -> (mpp |+| prefInline)
              }
          }

        case v =>
          // What does it mean actually? I've no ides
          State.pure((v, propertyPrefix))
      }
    }
  }

  private def unfoldProperties[S: Mangler: Exports: Arrows](
    properties: Chain[PropertyRaw]
  ): State[S, (Chain[PropertyModel], Inline)] = {
    properties
      .foldLeft[State[S, (Chain[PropertyModel], Inline)]](
        State.pure((Chain.empty[PropertyModel], Inline.empty))
      ) { case (pcm, p) =>
        pcm.flatMap { case (pc, m) =>
          unfoldProperty(p).map { case (pm, mm) =>
            (pc :+ pm, m |+| mm)
          }
        }
      }
  }

  private def unfoldRawWithProperties[S: Mangler: Exports: Arrows](
    raw: ValueRaw,
    properties: Chain[PropertyRaw],
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    ((raw, properties.headOption) match {
      case (v @ VarRaw(name, st @ StreamType(el)), Some(IntoIndexRaw(idx, _))) =>
        Mangler[S].findAndForbidName(name + "_gate").flatMap { gateName =>
          Mangler[S].findAndForbidName(name + "_idx").flatMap { idxName =>
            val gateVm = VarModel(gateName, ArrayType(el))
            val gateRaw = ApplyGateRaw(name, st, idxName, idx.`type`)
            unfoldProperties(properties).flatMap { case (propertyModels, map) =>
              reachModelWithPropertyModels(
                gateVm,
                propertyModels,
                map |+| Inline.preload(idxName -> idx, gateName -> gateRaw),
                propertiesAllowed
              )
            }
          }

        }

      case (_, _) =>
        unfold(raw).flatMap { case (vm, prevInline) =>
          unfoldProperties(properties).flatMap { case (propertyModels, map) =>
            reachModelWithPropertyModels(vm, propertyModels, prevInline |+| map, propertiesAllowed)
          }
        }
    })

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
      unfoldRawWithProperties(raw, properties, propertiesAllowed)
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
          vmLeftInline <- unfoldRawWithProperties(raw, left, propertiesAllowed)
          (leftVM, leftInline) = vmLeftInline
          fRaw = ApplyFunctorRaw(leftVM.toRaw, functor)
          vmFunctorInline <- ApplyFunctorRawInliner(fRaw, false)
          (fVM, fInline) = vmFunctorInline
          vmRightInline <- unfold(ApplyPropertyRaw.fromChain(fVM.toRaw, right), propertiesAllowed)
          (vm, rightInline) = vmRightInline
        } yield {
          vm -> (leftInline |+| fInline |+| rightInline)
        }
      }.getOrElse(unfoldRawWithProperties(raw, properties, propertiesAllowed))
    }

}
