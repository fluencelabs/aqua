/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.model.inline.raw

import aqua.errors.Errors.internalError
import aqua.model.*
import aqua.model.ValueModel.Ability
import aqua.model.inline.Inline
import aqua.model.inline.Inline.MergeMode.*
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.state.*
import aqua.raw.value.*
import aqua.types.*

import cats.Eval
import cats.data.{Chain, IndexedStateT, State}
import cats.instances.list.*
import cats.syntax.applicative.*
import cats.syntax.bifunctor.*
import cats.syntax.foldable.*
import cats.syntax.monoid.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import scribe.Logging

object ApplyPropertiesRawInliner extends RawInliner[ApplyPropertyRaw] with Logging {

  // in perspective literals can have properties and functors (like `nil` with length)
  def flatLiteralWithProperties[S: Mangler: Exports: Arrows: Config](
    literal: LiteralModel,
    inl: Inline,
    properties: Chain[PropertyModel]
  ): State[S, (VarModel, Inline)] = {
    for {
      apName <- Mangler[S].findAndForbidName("literal_ap")
      resultName <- Mangler[S].findAndForbidName(s"literal_props")
    } yield {
      val typ = literal.`type`
      val apVar = VarModel(apName, typ, properties)
      val tree = inl |+| Inline.tree(
        SeqModel.wrap(
          FlattenModel(literal.copy(`type` = typ), apVar.name).leaf,
          FlattenModel(apVar, resultName).leaf
        )
      )
      VarModel(resultName, properties.lastOption.map(_.`type`).getOrElse(typ)) -> tree
    }
  }

  private def unfoldAbilityProperty[S: Mangler: Exports: Arrows: Config](
    varModel: VarModel,
    abilityType: NamedType,
    p: PropertyRaw
  ): State[S, (VarModel, Inline)] = p match {
    case IntoArrowRaw(arrowName, _, arguments) =>
      val arrowType = abilityType.fields
        .lookup(arrowName)
        .collect { case at @ ArrowType(_, _) =>
          at
        }
        .getOrElse {
          internalError(
            s"Inlining, cannot find arrow ($arrowName) in ($varModel)"
          )
        }
      for {
        callArrow <- CallArrowRawInliner(
          CallArrowRaw.func(
            funcName = AbilityType.fullName(varModel.name, arrowName),
            baseType = arrowType,
            arguments = arguments
          )
        )
        result <- callArrow match {
          case (vm: VarModel, inl) =>
            State.pure((vm, inl))
          case (lm: LiteralModel, inl) =>
            flatLiteralWithProperties(lm, inl, Chain.empty).flatMap { case (vm, inline) =>
              Exports[S].resolved(vm.name, vm).map(_ => (vm, inline))
            }
        }
      } yield result
    case IntoFieldRaw(fieldName, at @ AbilityType(_, _)) =>
      (VarModel(AbilityType.fullName(varModel.name, fieldName), at), Inline.empty).pure
    case IntoFieldRaw(fieldName, _) =>
      for {
        abilityField <- Exports[S].getAbilityField(varModel.name, fieldName)
        result <- abilityField match {
          case Some(vm: VarModel) =>
            State.pure((vm, Inline.empty))
          case Some(lm: LiteralModel) =>
            flatLiteralWithProperties(lm, Inline.empty, Chain.empty)
          case _ =>
            Exports[S].getKeys.flatMap { keys =>
              val field = AbilityType.fullName(varModel.name, fieldName)
              internalError(
                s"Inlining, cannot find field ($field) in ability $varModel. Available: $keys"
              )
            }

        }
      } yield result
    case icr: IntoCopyRaw =>
      internalError(
        s"Inlining, cannot use 'copy' ($icr) in ability ($varModel)"
      )
    case fr: FunctorRaw =>
      internalError(
        s"Inlining, cannot use functor ($fr) in ability ($varModel)"
      )
    case iir: IntoIndexRaw =>
      internalError(
        s"Inlining, cannot use index ($iir) in ability ($varModel)"
      )
  }

  private[inline] def unfoldProperty[S: Mangler: Exports: Arrows: Config](
    varModel: VarModel,
    p: PropertyRaw
  ): State[S, (VarModel, Inline)] =
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
          flattenVI <- removeProperties(varModel)
          (flatten, inline) = flattenVI
          newVI <- ApplyFunctorRawInliner(flatten, f)
        } yield {
          newVI._1 -> Inline(
            inline.predo ++ newVI._2.predo,
            mergeMode = SeqMode
          )
        }

      case ic @ IntoCopyRaw(_, _) =>
        for {
          flattenVI <- removeProperties(varModel)
          (flatten, inline) = flattenVI
          newVI <- ApplyIntoCopyRawInliner(flatten, ic)
        } yield {
          newVI._1 -> Inline(
            Chain.one(SeqModel.wrap(inline.predo ++ newVI._2.predo)),
            mergeMode = SeqMode
          )
        }
      case iar: IntoArrowRaw =>
        internalError(
          s"Inlining, cannot use arrow ($iar) in non-ability type ($varModel)"
        )
    }

  // Helper for `optimizeProperties`
  private case class PropertyRawWithModel(raw: PropertyRaw, model: Option[PropertyModel])

  // Unfold properties that we can process in parallel
  private def optimizeProperties[S: Mangler: Exports: Arrows: Config](
    properties: Chain[PropertyRaw]
  ): State[S, (Chain[PropertyRawWithModel], Inline)] = {
    properties.map {
      case iir @ IntoIndexRaw(vr, t) =>
        unfold(vr, propertiesAllowed = false).flatMap {
          case (vm @ VarModel(_, _, _), inline) if vm.properties.nonEmpty =>
            removeProperties(vm).map { case (vf, inlf) =>
              PropertyRawWithModel(
                iir,
                IntoIndexModel
                  .fromValueModel(vf, t)
                  .getOrElse(
                    internalError(s"Unexpected: could not convert ($vf) to IntoIndexModel")
                  )
                  .some
              ) -> Inline(
                inline.predo ++ inlf.predo,
                mergeMode = SeqMode
              )
            }
          case (vm, inline) =>
            (
              PropertyRawWithModel(
                iir,
                IntoIndexModel
                  .fromValueModel(vm, t)
                  .getOrElse(
                    internalError(s"Unexpected: could not convert ($vm) to IntoIndexModel")
                  )
                  .some
              ) -> inline
            ).pure
        }

      case p => State.pure(PropertyRawWithModel(p, None) -> Inline.empty)
    }.sequence.map(_.toList.unzip.bimap(Chain.fromSeq, _.combineAll))
  }

  private def unfoldProperties[S: Mangler: Exports: Arrows: Config](
    prevInline: Inline,
    vm: VarModel,
    properties: Chain[PropertyRaw],
    propertiesAllowed: Boolean
  ): State[S, (VarModel, Inline)] = {
    optimizeProperties(properties).flatMap { case (optimizedProps, optimizationInline) =>
      optimizedProps
        .foldLeft[State[S, (VarModel, Inline)]](
          State.pure((vm, prevInline.mergeWith(optimizationInline, SeqMode)))
        ) { case (state, property) =>
          state.flatMap {
            case (vm @ Ability(_, at), leftInline) =>
              unfoldAbilityProperty(vm, at, property.raw).map { case (vm, inl) =>
                (
                  vm,
                  Inline(
                    leftInline.predo ++ inl.predo,
                    mergeMode = SeqMode
                  )
                )
              }
            case (vm, leftInline) =>
              property match {
                case PropertyRawWithModel(_, Some(model)) =>
                  State.pure(vm.copy(properties = vm.properties :+ model) -> leftInline)
                case PropertyRawWithModel(raw, _) =>
                  unfoldProperty(vm, raw).flatMap {
                    case (v, i) if !propertiesAllowed && v.properties.nonEmpty =>
                      removeProperties(v).map { case (vf, inlf) =>
                        vf -> Inline(
                          leftInline.predo ++ i.predo ++ inlf.predo,
                          mergeMode = SeqMode
                        )
                      }
                    case (v, i) =>
                      State.pure(
                        v -> Inline(
                          leftInline.predo ++ i.predo,
                          mergeMode = SeqMode
                        )
                      )
                  }
              }
          }
        }
    }
  }

  /**
   * Unfold `stream[idx]`
   */
  private def unfoldStreamGate[S: Mangler: Exports: Arrows: Config](
    streamName: String,
    streamType: StreamType,
    idx: ValueRaw
  ): State[S, (VarModel, Inline)] = for {
    /**
     * Inline size, which is `idx + 1`
     * Increment on ValueRaw level to
     * apply possible optimizations
     */
    sizeInlined <- unfold(idx.increment)
    (sizeVM, sizeInline) = sizeInlined
    /**
     * Inline idx which is `size - 1`
     * TODO: Do not generate it if
     * it is not needed, e.g. in `join`
     */
    idxInlined <- sizeVM match {
      /**
       * Micro optimization: if idx is a literal
       * do not generate inline.
       */
      case LiteralModel.Integer(i, t) =>
        (LiteralModel((i - 1).toString, t), Inline.empty).pure[State[S, *]]
      case _ =>
        Mangler[S].findAndForbidName(s"${streamName}_idx").map { idxName =>
          val idxVar = VarModel(idxName, sizeVM.`type`)
          val idxInline = Inline.tree(
            CallServiceModel(
              "math",
              funcName = "sub",
              args = List(sizeVM, LiteralModel.number(1)),
              result = idxVar
            ).leaf
          )

          (idxVar, idxInline)
        }
    }
    (idxVM, idxInline) = idxInlined
    /**
     * Inline join of `size` elements of stream
     */
    gateInlined <- StreamGateInliner(streamName, streamType, sizeVM)
    (gateVM, gateInline) = gateInlined
    /**
     * Construct stream[idx]
     */
    gate = gateVM.withProperty(
      IntoIndexModel
        .fromValueModel(idxVM, streamType.element)
        .getOrElse(
          internalError(s"Unexpected: could not convert ($idxVM) to IntoIndexModel")
        )
    )
  } yield gate -> Inline(
    sizeInline.predo ++
      gateInline.predo ++
      idxInline.predo,
    mergeMode = SeqMode
  )

  private def unfoldRawWithProperties[S: Mangler: Exports: Arrows: Config](
    raw: ValueRaw,
    properties: Chain[PropertyRaw],
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] =
    (raw, properties.uncons) match {
      /**
       * To inline
       */
      case (
            vr @ VarRaw(_, st @ StreamType(_)),
            Some(IntoIndexRaw(idx, _), otherProperties)
          ) =>
        unfold(vr).flatMap {
          case (VarModel(nameVM, _, _), inl) =>
            for {
              gateInlined <- unfoldStreamGate(nameVM, st, idx)
              (gateVM, gateInline) = gateInlined
              propsInlined <- unfoldProperties(
                gateInline,
                gateVM,
                otherProperties,
                propertiesAllowed
              )
            } yield propsInlined
          case _ =>
            internalError(
              s"Unfolded stream ($vr) cannot be a literal"
            )
        }
      case (
            vr @ VarRaw(_, st @ StreamMapType(_)),
            Some(iar @ IntoArrowRaw(arrowName, _, args), otherProperties)
          ) =>
        unfold(vr).flatMap {
          case (VarModel(nameVM, _, _), inl) =>
            for {
              argsInlined <- args.traverse(unfold(_)).map(_.unzip)
              (argsVM, argsInline) = argsInlined
              resultInlined <- ApplyStreamMapRawInliner(arrowName, nameVM, st, argsVM)
              (resultVM, resultInline) = resultInlined
              propsInlined <- unfoldProperties(
                (argsInline :+ resultInline).combineAll,
                resultVM,
                otherProperties,
                propertiesAllowed
              )
            } yield propsInlined
          case _ =>
            internalError(
              s"Unfolded stream map ($vr) cannot be a literal"
            )
        }

      case (_, _) =>
        unfold(raw).flatMap {
          case (vm: VarModel, prevInline) =>
            unfoldProperties(prevInline, vm, properties, propertiesAllowed)
              // To coerce types
              .map(identity)
          case (l: LiteralModel, inline) =>
            flatLiteralWithProperties(
              l,
              inline,
              Chain.empty
            ).flatMap { (varModel, prevInline) =>
              unfoldProperties(prevInline, varModel, properties, propertiesAllowed).map {
                case (v, i) =>
                  v -> i
              }
            }
        }
    }

  /**
   * Remove properties from the var and return a new var without them
   */
  private def removeProperties[S: Mangler](
    varModel: VarModel
  ): State[S, (VarModel, Inline)] =
    if (varModel.properties.isEmpty) (varModel, Inline.empty).pure
    else
      for {
        nn <- Mangler[S].findAndForbidName(varModel.name + "_flat")
        flatten = VarModel(nn, varModel.`type`)
      } yield flatten -> Inline.tree(FlattenModel(varModel, flatten.name).leaf)

  override def apply[S: Mangler: Exports: Arrows: Config](
    apr: ApplyPropertyRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    val (raw, properties) = apr.unwind
    unfoldRawWithProperties(raw, properties, propertiesAllowed)
  }
}
