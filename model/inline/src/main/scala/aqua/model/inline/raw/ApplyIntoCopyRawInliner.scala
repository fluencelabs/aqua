package aqua.model.inline.raw

import aqua.model.{CallModel, CallServiceModel, LiteralModel, OpModel, SeqModel, ValueModel, VarModel}
import aqua.model.inline.{Inline, SeqMode}
import aqua.model.inline.MakeStructRawInliner.createObj
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{ApplyIntoCopyRaw, LiteralRaw}
import aqua.types.ScalarType
import cats.data.{Chain, NonEmptyMap, State}
import scribe.Logging
import cats.syntax.traverse.*
import cats.syntax.monoid.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.apply.*

object ApplyIntoCopyRawInliner extends RawInliner[ApplyIntoCopyRaw] with Logging {

  private def copyObj(value: VarModel, fields: NonEmptyMap[String, ValueModel], result: VarModel): OpModel.Tree = {
    val args = fields.toSortedMap.toList.flatMap { case (name, value) =>
      LiteralModel.fromRaw(LiteralRaw.quote(name)) :: value :: Nil
    }
    CallServiceModel(
      LiteralModel("\"json\"", ScalarType.string),
      "puts",
      CallModel(
        value +: args,
        CallModel.Export(result.name, result.`type`) :: Nil
      )
    ).leaf
  }

  override def apply[S: Mangler: Exports: Arrows](
    aicr: ApplyIntoCopyRaw,
    propertyAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    unfold(aicr.value).flatMap {
      case (v @ VarModel(name, _, _), inl) =>
        for {
          name <- Mangler[S].findAndForbidName(name + "_obj_copy")
          foldedFields <- aicr.intoCopy.fields.nonEmptyTraverse(unfold(_))
        } yield {
          val varModel = VarModel(name, aicr.baseType)
          val valsInline = foldedFields.toSortedMap.values.map(_._2).fold(Inline.empty)(_ |+| _)
          val fields = foldedFields.map(_._1)
          val objCreation = copyObj(v, fields, varModel)
          (
            varModel,
            Inline(
              inl.flattenValues ++ valsInline.flattenValues,
              Chain.one(SeqModel.wrap(((inl.predo ++ valsInline.predo) :+ objCreation).toList: _*)),
              SeqMode
            )
          )
        }
      case v =>
        // unexpected, properties are prohibited for literals
        logger.error(s"Unexpected. Copy operation is prohibited for literals. Literal: '$v'")
        State.pure(v)
    }

  }
}
