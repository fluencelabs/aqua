package aqua.model.inline.raw

import aqua.model.{
  CallModel,
  CallServiceModel,
  LiteralModel,
  OpModel,
  SeqModel,
  ValueModel,
  VarModel
}
import aqua.model.inline.{Inline, SeqMode}
import aqua.model.inline.MakeStructRawInliner.createObj
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{IntoCopyRaw, LiteralRaw}
import aqua.types.ScalarType
import cats.data.{Chain, NonEmptyMap, State}
import scribe.Logging
import cats.syntax.traverse.*
import cats.syntax.monoid.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.apply.*

object ApplyIntoCopyRawInliner extends Logging {

  private def copyObj(
    value: VarModel,
    fields: NonEmptyMap[String, ValueModel],
    result: VarModel
  ): OpModel.Tree = {
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

  def apply[S: Mangler: Exports: Arrows](
    value: VarModel,
    intoCopy: IntoCopyRaw
  ): State[S, (VarModel, Inline)] = {
    for {
      name <- Mangler[S].findAndForbidName(value.name + "_obj_copy")
      foldedFields <- intoCopy.fields.nonEmptyTraverse(unfold(_))
    } yield {
      val varModel = VarModel(name, value.baseType)
      val valsInline = foldedFields.toSortedMap.values.map(_._2).fold(Inline.empty)(_ |+| _)
      val fields = foldedFields.map(_._1)
      val objCreation = copyObj(value, fields, varModel)
      (
        varModel,
        Inline(
          valsInline.flattenValues,
          Chain.one(SeqModel.wrap((valsInline.predo :+ objCreation).toList: _*)),
          SeqMode
        )
      )
    }

  }
}
