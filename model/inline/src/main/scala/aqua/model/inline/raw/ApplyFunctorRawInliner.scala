package aqua.model.inline.raw

import aqua.model.{
  CallModel,
  CanonicalizeModel,
  FlattenModel,
  FunctorModel,
  LiteralModel,
  SeqModel,
  ValueModel,
  VarModel
}
import aqua.model.inline.{Inline, SeqMode}
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{FunctorRaw, ValueRaw}
import cats.data.State
import cats.data.Chain
import aqua.model.inline.RawValueInliner.unfold
import aqua.types.{ArrayType, BoxType, CanonStreamType, StreamType}
import cats.syntax.monoid.*
import scribe.Logging

object ApplyFunctorRawInliner extends Logging {

  def apply[S: Mangler: Exports: Arrows](
    value: ValueModel,
    functor: FunctorRaw
  ): State[S, (VarModel, Inline)] = {
    val functorModel = FunctorModel(functor.name, functor.`type`)

    value match {
      case v @ VarModel(name, bt, _) =>
        for {
          apName <- Mangler[S].findAndForbidName(name + "_to_functor")
          resultName <- Mangler[S].findAndForbidName(s"${name}_${functor.name}")
          (apVar, flat) = {
            bt match {
              case StreamType(el) =>
                val canonType = CanonStreamType(el)
                (
                  VarModel(apName, canonType, Chain.one(functorModel)),
                  CanonicalizeModel(v, CallModel.Export(apName, canonType)).leaf
                )
              case _ =>
                (VarModel(apName, functorModel.`type`, Chain.one(functorModel)), FlattenModel(v, apName).leaf)
            }
          }
        } yield {
          val tree = Inline(
            predo = Chain.one(SeqModel.wrap(
              flat,
              FlattenModel(apVar, resultName).leaf
            )),
            mergeMode = SeqMode
          )

          VarModel(resultName, functor.`type`) -> tree
        }
      case l @ LiteralModel(_, _) =>
        ApplyPropertiesRawInliner.flatLiteralWithProperties(
          l,
          Inline.empty,
          Chain.one(functorModel)
        )
    }
  }
}
