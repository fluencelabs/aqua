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
  OpModel,
  PropertyModel,
  PushToStreamModel,
  RestrictionModel,
  SeqModel,
  ValueModel,
  VarModel,
  XorModel
}
import aqua.model.inline.Inline
import aqua.model.inline.SeqMode
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{
  ApplyGateRaw,
  ApplyPropertyRaw,
  CallArrowRaw,
  FunctorRaw,
  IntoCopyRaw,
  IntoFieldRaw,
  IntoIndexRaw,
  LiteralRaw,
  PropertyRaw,
  ValueRaw,
  VarRaw
}
import aqua.types.{ArrayType, CanonStreamType, ScalarType, StreamType, Type}
import cats.Eval
import cats.data.{Chain, IndexedStateT, State}
import cats.syntax.monoid.*
import cats.instances.list.*
import scribe.Logging

object ApplyPropertiesRawInliner extends RawInliner[ApplyPropertyRaw] with Logging {

  // in perspective literals can have properties and functors (like `nil` with length)
  def flatLiteralWithProperties[S: Mangler: Exports: Arrows](
    literal: LiteralModel,
    inl: Inline,
    properties: Chain[PropertyModel],
    resultType: Type
  ): State[S, (VarModel, Inline)] = {
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

  private def flatVar[S: Mangler: Exports: Arrows](
    varModel: VarModel
  ): State[S, (VarModel, Inline)] = {
    for {
      nn <- Mangler[S].findAndForbidName(varModel.name + "_flat")
    } yield {
      val flatten = VarModel(nn, varModel.`type`)
      flatten -> Inline.tree(FlattenModel(varModel, flatten.name).leaf)
    }
  }

  private[inline] def unfoldProperty[S: Mangler: Exports: Arrows](
    varModel: VarModel,
    p: PropertyRaw
  ): State[S, (VarModel, Inline)] = // TODO property for collection
    p match {
      case IntoFieldRaw(field, t) =>
        State.pure(
          varModel.copy(properties =
            varModel.properties :+ IntoFieldModel(field, t)
          ) -> Inline.empty
        )

      case IntoIndexRaw(LiteralRaw(value, _), t) =>
        State.pure(
          varModel.copy(properties =
            varModel.properties :+ IntoIndexModel(value, t)
          ) -> Inline.empty
        )

      case IntoIndexRaw(vr, t) =>
        unfold(vr, propertiesAllowed = false).map {
          case (VarModel(name, _, _), inline) =>
            varModel.copy(properties = varModel.properties :+ IntoIndexModel(name, t)) -> inline
          case (LiteralModel(literal, _), inline) =>
            varModel.copy(properties = varModel.properties :+ IntoIndexModel(literal, t)) -> inline
        }

      case f @ FunctorRaw(_, _) =>
        for {
          flattenVI <- if (varModel.properties.nonEmpty) flatVar(varModel) else State.pure(varModel, Inline.empty)
          (flatten, inline) = flattenVI
          newVI <- ApplyFunctorRawInliner(flatten, f)
        } yield {
          newVI._1 -> Inline(
            inline.flattenValues ++ newVI._2.flattenValues,
            inline.predo ++ newVI._2.predo,
            mergeMode = SeqMode
          )
        }

      case ic @ IntoCopyRaw(_, _) =>
        for {
          flattenVI <- if (varModel.properties.nonEmpty) flatVar(varModel) else State.pure(varModel, Inline.empty)
          (flatten, inline) = flattenVI
          newVI <- ApplyIntoCopyRawInliner(varModel, ic)
        } yield {
          newVI._1 -> Inline(
            inline.flattenValues ++ newVI._2.flattenValues,
            inline.predo ++ newVI._2.predo,
            mergeMode = SeqMode
          )
        }

    }

  // case l: LiteralModel if propertyModels.nonEmpty =>
  //          flatLiteralWithProperties(l, propertyPrefix, propertyModels, propertyModels.lastOption.map(_.`type`).getOrElse(l.`type`))

  private def unfoldProperties[S: Mangler: Exports: Arrows](
    prevInline: Inline,
    vm: VarModel,
    properties: Chain[PropertyRaw],
    propertiesAllowed: Boolean
  ): State[S, (VarModel, Inline)] = {
    properties
      .foldLeft[State[S, (VarModel, Inline)]](
        State.pure((vm, prevInline))
      ) { case (state, property) =>
        state.flatMap { case (vm, leftInline) =>
          unfoldProperty(vm, property).flatMap {
            case (v, i) if !propertiesAllowed && v.properties.nonEmpty =>
              flatVar(v).map { case (vf, inlf) =>
                vf -> Inline(
                  leftInline.flattenValues ++ i.flattenValues ++ inlf.flattenValues,
                  leftInline.predo ++ i.predo ++ inlf.predo,
                  mergeMode = SeqMode
                )
              }
            case (v, i) =>
              State.pure(
                v -> Inline(
                  leftInline.flattenValues ++ i.flattenValues,
                  leftInline.predo ++ i.predo,
                  mergeMode = SeqMode
                )
              )
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
          case (vm @ VarModel(nameVM, _, _), inl) =>
            val gateRaw = ApplyGateRaw(nameVM, st, idx)
            unfold(gateRaw).flatMap {
              case (gateResVal: VarModel, gateResInline) =>
                unfoldProperties(gateResInline, gateResVal, properties, propertiesAllowed).map {
                  case (v, i) =>
                    (v: ValueModel) -> Inline(
                      inl.flattenValues ++ i.flattenValues,
                      inl.predo ++ i.predo,
                      mergeMode = SeqMode
                    )
                }
              case (v, i) =>
                logger.error("Unreachable. Unfolded stream cannot be a literal")
                State.pure(v -> i)
            }
          case l =>
            logger.error("Unreachable. Unfolded stream cannot be a literal")
            State.pure(l)
        }

      case (_, _) =>
        unfold(raw).flatMap {
          case (vm: VarModel, prevInline) =>
            unfoldProperties(prevInline, vm, properties, propertiesAllowed).map { case (v, i) =>
              (v: ValueModel) -> i
            }
          case (v, i) =>
            logger.error("Unexpected. Unfolded stream cannot be a literal")
            State.pure(v -> i)
        }
    })

  }

  override def apply[S: Mangler: Exports: Arrows](
    apr: ApplyPropertyRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    val (raw, properties) = apr.unwind
    unfoldRawWithProperties(raw, properties, propertiesAllowed)
  }
}
