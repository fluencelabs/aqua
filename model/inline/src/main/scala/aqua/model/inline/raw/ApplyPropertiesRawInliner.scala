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
import aqua.model.inline.{ParMode, SeqMode}
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.state.{Arrows, Exports, Mangler, Scopes}
import aqua.raw.value.{
  ApplyGateRaw,
  ApplyPropertyRaw,
  CallArrowRaw,
  FunctorRaw,
  IntoArrowRaw,
  IntoCopyRaw,
  IntoFieldRaw,
  IntoIndexRaw,
  LiteralRaw,
  PropertyRaw,
  ValueRaw,
  VarRaw
}
import aqua.types.{ArrayType, CanonStreamType, ScalarType, ScopeType, StreamType, Type}
import cats.Eval
import cats.data.{Chain, IndexedStateT, State}
import cats.syntax.monoid.*
import cats.syntax.traverse.*
import cats.instances.list.*
import scribe.Logging

object ApplyPropertiesRawInliner extends RawInliner[ApplyPropertyRaw] with Logging {

  // in perspective literals can have properties and functors (like `nil` with length)
  def flatLiteralWithProperties[S: Mangler: Exports: Arrows](
    literal: LiteralModel,
    inl: Inline,
    properties: Chain[PropertyModel]
  ): State[S, (VarModel, Inline)] = {
    for {
      apName <- Mangler[S].findAndForbidName("literal_ap")
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
      VarModel(resultName, properties.lastOption.map(_.`type`).getOrElse(cleanedType)) -> tree
    }
  }

  private def removeProperties[S: Mangler](
    varModel: VarModel
  ): State[S, (VarModel, Inline)] = {
    for {
      nn <- Mangler[S].findAndForbidName(varModel.name + "_flat")
    } yield {
      val flatten = VarModel(nn, varModel.`type`)
      flatten -> Inline.tree(FlattenModel(varModel, flatten.name).leaf)
    }
  }

  private def unfoldScopeProperty[S: Mangler: Exports: Arrows: Scopes](
    varModel: VarModel,
    scopeType: ScopeType,
    p: PropertyRaw
  ): State[S, (VarModel, Inline)] = {
    p match {
      case IntoArrowRaw(arrowName, t, arguments) =>
        println("find arrow for: " + varModel)
        println("arrow name: " + arrowName)
        println("arrow type:" + t)
        for {
          _ <- Scopes[S].scopes.map { sc =>
            println("scopes: " + sc)
          }
          arrowOp <- Scopes[S].getArrow(varModel.name, arrowName)
          _ <- Arrows[S].arrows.map { arrs =>
            println(arrs.map(_._2.funcName))
          }
          arrow = arrowOp.get
          callArrow <- CallArrowRawInliner(
            CallArrowRaw(None, arrow.funcName, arguments, arrow.arrowType, None)
          )
          result <- callArrow match {
            case (vm: VarModel, inl) =>
              State.pure((vm, inl))
            case (lm: LiteralModel, inl) =>
              flatLiteralWithProperties(lm, inl, Chain.empty).flatMap { case (vm, inline) =>
                Exports[S].resolved(vm.name, vm).map(_ => (vm, inline))
              }
          }
        } yield {
          result
        }

      case IntoFieldRaw(fieldName, t) =>
        println("find field for: " + varModel)
        println("field name: " + fieldName)
        println("field type:" + t)
        for {
          // TODO: we cannot have scope as argument in `Scopes` in exported functions
          valueOp <- Scopes[S].getValue(varModel.name, fieldName)
          value = valueOp.get
          result <- value match {
            case vm: VarModel =>
              State.pure((vm, Inline.empty))
            case lm: LiteralModel =>
              flatLiteralWithProperties(lm, Inline.empty, Chain.empty)
          }
        } yield {
          result
        }
    }
  }

  private[inline] def unfoldProperty[S: Mangler: Exports: Arrows: Scopes](
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
          flattenVI <-
            if (varModel.properties.nonEmpty) removeProperties(varModel)
            else State.pure(varModel, Inline.empty)
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
          flattenVI <-
            if (varModel.properties.nonEmpty) removeProperties(varModel)
            else State.pure(varModel, Inline.empty)
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

  // Helper for `optimizeProperties`
  private case class PropertyRawWithModel(raw: PropertyRaw, model: Option[PropertyModel])

  // Unfold properties that we can process in parallel
  private def optimizeProperties[S: Mangler: Exports: Arrows: Scopes](
    properties: Chain[PropertyRaw]
  ): State[S, (Chain[PropertyRawWithModel], Inline)] = {
    properties.map {
      case iir @ IntoIndexRaw(vr, t) =>
        unfold(vr, propertiesAllowed = false).flatMap {
          case (vm @ VarModel(_, _, _), inline) if vm.properties.nonEmpty =>
            removeProperties(vm).map { case (vf, inlf) =>
              PropertyRawWithModel(iir, Option(IntoIndexModel(vf.name, t))) -> Inline(
                inline.flattenValues ++ inlf.flattenValues,
                inline.predo ++ inlf.predo,
                mergeMode = SeqMode
              )
            }
          case (VarModel(name, _, _), inline) =>
            State.pure(PropertyRawWithModel(iir, Option(IntoIndexModel(name, t))) -> inline)
          case (LiteralModel(literal, _), inline) =>
            State.pure(PropertyRawWithModel(iir, Option(IntoIndexModel(literal, t))) -> inline)
        }

      case p => State.pure(PropertyRawWithModel(p, None) -> Inline.empty)
    }.sequence.map { (propsWithInline: Chain[(PropertyRawWithModel, Inline)]) =>
      val fullInline = propsWithInline.map(_._2).foldLeft(Inline.empty)(_ |+| _)
      val props = propsWithInline.map(_._1)
      (props, fullInline)
    }
  }

  private def unfoldProperties[S: Mangler: Exports: Arrows: Scopes](
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
            case (vm@VarModel(name, st@ScopeType(_, _), _), leftInline) =>
              unfoldScopeProperty(vm, st, property.raw).map {
                case (vm, inl) => (vm, Inline(
                  leftInline.flattenValues ++ inl.flattenValues,
                  leftInline.predo ++ inl.predo,
                  mergeMode = SeqMode
                ))
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
    }
  }

  private def unfoldRawWithProperties[S: Mangler: Exports: Arrows: Scopes](
    raw: ValueRaw,
    properties: Chain[PropertyRaw],
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    ((raw, properties.headOption) match {
      case (vr @ VarRaw(_, st @ StreamType(_)), Some(IntoIndexRaw(idx, _))) =>
        unfold(vr).flatMap {
          case (VarModel(nameVM, _, _), inl) =>
            val gateRaw = ApplyGateRaw(nameVM, st, idx)
            unfold(gateRaw).flatMap {
              case (gateResVal: VarModel, gateResInline) =>
                unfoldProperties(gateResInline, gateResVal, properties, propertiesAllowed).map {
                  case (v, i) =>
                    v -> Inline(
                      inl.flattenValues ++ i.flattenValues,
                      inl.predo ++ i.predo,
                      mergeMode = SeqMode
                    )
                }
              case (v, i) =>
                // what if pass nil as stream argument?
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
              v -> i
            }
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
    })

  }

  override def apply[S: Mangler: Exports: Arrows: Scopes](
    apr: ApplyPropertyRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    val (raw, properties) = apr.unwind
    unfoldRawWithProperties(raw, properties, propertiesAllowed)
  }
}
