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
import aqua.model.inline.Inline
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.ApplyFunctorRaw
import cats.data.State
import cats.data.Chain
import aqua.model.inline.RawValueInliner.unfold
import aqua.types.{BoxType, CanonStreamType, StreamType, ArrayType}
import cats.syntax.monoid.*
import scribe.Logging

object ApplyFunctorRawInliner extends RawInliner[ApplyFunctorRaw] with Logging {

  override def apply[S: Mangler: Exports: Arrows](
    afr: ApplyFunctorRaw,
    propertyAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    val functorModel = FunctorModel(afr.functor.name, afr.functor.`type`)

    unfold(afr.value).flatMap {
      case (v @ VarModel(name, bt, _), inl) =>
        for {
          apName <- Mangler[S].findAndForbidName(name + "_to_functor")
          resultName <- Mangler[S].findAndForbidName(s"${name}_${afr.functor.name}")
          (apVar, flat) = {
            bt match {
              case StreamType(el) =>
                val canonType = CanonStreamType(el)
                (
                  VarModel(apName, canonType, Chain.one(functorModel)),
                  CanonicalizeModel(v, CallModel.Export(apName, canonType)).leaf
                )
              case _ =>
                (VarModel(apName, bt, Chain.one(functorModel)), FlattenModel(v, apName).leaf)
            }
          }
        } yield {
          val tree = inl |+| Inline.tree(
            SeqModel.wrap(
              flat,
              FlattenModel(apVar, resultName).leaf
            )
          )

          VarModel(resultName, afr.functor.`type`) -> tree
        }
      case (l @ LiteralModel(_, _), inl) =>
        ApplyPropertiesRawInliner.literalWithProperties(l, inl, Chain.one(functorModel), afr.functor.`type`)
      case v =>
        // unexpected, properties are prohibited for literals
        logger.error(s"Unexpected. Properties are prohibited for literals. Literal: '$v'")
        State.pure(v)
    }
  }
}
