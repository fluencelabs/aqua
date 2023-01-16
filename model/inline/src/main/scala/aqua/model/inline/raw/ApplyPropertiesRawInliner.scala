package aqua.model.inline.raw

import aqua.model.{CallModel, CallServiceModel, CanonicalizeModel, FlattenModel, ForModel, FunctorModel, IntoFieldModel, IntoIndexModel, LiteralModel, MatchMismatchModel, NextModel, PropertyModel, PushToStreamModel, RestrictionModel, SeqModel, ValueModel, VarModel, XorModel}
import aqua.model.inline.Inline
import aqua.model.inline.SeqMode
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{ApplyFunctorRaw, ApplyGateRaw, ApplyPropertyRaw, CallArrowRaw, FunctorRaw, IntoFieldRaw, IntoIndexRaw, LiteralRaw, PropertyRaw, ValueRaw, VarRaw}
import aqua.types.{ArrayType, CanonStreamType, ScalarType, StreamType, Type}
import cats.Eval
import cats.data.{Chain, IndexedStateT, State}
import cats.syntax.monoid.*
import cats.instances.list.*

object ApplyPropertiesRawInliner extends RawInliner[ApplyPropertyRaw] {

  // in perspective literals can have properties and functors (like `nil` with length)
  def literalWithProperties[S: Mangler: Exports: Arrows](
    literal: LiteralModel,
    inl: Inline,
    properties: Chain[PropertyModel],
    resultType: Type
  ): State[S, (ValueModel, Inline)] = {
    for {
      apName <- Mangler[S].findAndForbidName("literal_to_functor")
      resultName <- Mangler[S].findAndForbidName(s"literal_props")
    } yield {
      val cleanedType = literal.`type` match {
        // literals cannot be streams, use it as an array to use properties
        case StreamType(el) => ArrayType(el)
        case tt => tt
      }
      val apVar = VarModel(apName, cleanedType, properties)
      val tree = inl |+| Inline.tree(
        SeqModel.wrap(
          FlattenModel(literal.copy(`type` = cleanedType), apVar.name).leaf,
          FlattenModel(apVar, resultName).leaf
        )
      )
      VarModel(resultName, resultType) -> tree
    }
  }

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
          case (VarModel(name, _, _), inline) =>
            IntoIndexModel(name, t) -> inline
          case (LiteralModel(v, _), inline) => IntoIndexModel(v, t) -> inline
        }

      case IntoIndexRaw(LiteralRaw(value, _), t) =>
        State.pure(IntoIndexModel(value, t) -> Inline.empty)
    }

  def reachModelWithPropertyModels[S: Mangler: Exports: Arrows](
    model: ValueModel,
    propertyModels: Chain[PropertyModel],
    propertyPrefix: Inline,
    propertiesAllowed: Boolean,
    streamGateInline: Option[Inline] = None
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
                val resultInline = streamGateInline.map { gInline =>
                  Inline(
                    prefInline.flattenValues ++ mpp.flattenValues ++ gInline.flattenValues,
                    Chain.one(
                      SeqModel.wrap((prefInline.predo ++ mpp.predo ++ gInline.predo).toList: _*)
                    ),
                    SeqMode
                  )
                }.getOrElse(prefInline |+| mpp)
                vmm -> resultInline
              }
          }

        case l: LiteralModel if propertyModels.nonEmpty =>
          literalWithProperties(l, propertyPrefix, propertyModels, propertyModels.lastOption.map(_.`type`).getOrElse(l.`type`))

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
      // To wait for the element of a stream by the given index, the following model is generated:
      // (seq
      // (seq
      //  (seq
      //   (call %init_peer_id% ("math" "add") [0 1] stream_incr)
      //   (fold $stream s
      //    (seq
      //     (seq
      //      (ap s $stream_test)
      //      (canon %init_peer_id% $stream_test  #stream_iter_canon)
      //     )
      //     (xor
      //      (match #stream_iter_canon.length stream_incr
      //       (null)
      //      )
      //      (next s)
      //     )
      //    )
      //    (never)
      //   )
      //  )
      //  (canon %init_peer_id% $stream_test  #stream_result_canon)
      // )
      // (ap #stream_result_canon stream_gate)
      // )
      case (vr @ VarRaw(_, st @ StreamType(_)), Some(IntoIndexRaw(idx, _))) =>
        unfold(vr).flatMap {
          case (VarModel(nameVM, _, _), inl) =>
            val gateRaw = ApplyGateRaw(nameVM, st, idx)
            unfold(gateRaw).flatMap { case (gateResVal, gateResInline) =>
              unfoldProperties(properties).flatMap { case (propertyModels, map) =>
                reachModelWithPropertyModels(
                  gateResVal,
                  propertyModels,
                  inl |+| map,
                  false,
                  Some(gateResInline)
                )
              }
            }
          case l =>
            // unreachable. Stream cannot be literal
            State.pure(l)
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
          // TODO: rewrite without `toRaw`
          fRaw = ApplyFunctorRaw(leftVM.toRaw, functor)
          vmFunctorInline <- ApplyFunctorRawInliner(fRaw, false)
          (fVM, fInline) = vmFunctorInline
          // TODO: rewrite without `toRaw`
          vmRightInline <- unfold(ApplyPropertyRaw.fromChain(fVM.toRaw, right), propertiesAllowed)
          (vm, rightInline) = vmRightInline
        } yield {
          vm -> (leftInline |+| fInline |+| rightInline)
        }
      }.getOrElse(unfoldRawWithProperties(raw, properties, propertiesAllowed))
    }

}
