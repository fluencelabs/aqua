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
import aqua.model.inline.{Inline, SeqMode, TagInliner}
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

  private def copyObj[S: Mangler](
    value: VarModel,
    fields: NonEmptyMap[String, ValueModel],
    result: VarModel
  ): State[S, OpModel.Tree] = {
    fields.toSortedMap.toList.flatMap { case (name, value) =>
      LiteralModel.fromRaw(LiteralRaw.quote(name)) :: value :: Nil
    }.map(TagInliner.canonicalizeIfStream(_, None)).sequence.map { argsWithOps =>
      val (args, ops) = argsWithOps.unzip
      val copyOp = CallServiceModel(
        LiteralModel("\"json\"", ScalarType.string),
        "puts",
        CallModel(
          value +: args,
          CallModel.Export(result.name, result.`type`) :: Nil
        )
      ).leaf
      SeqModel.wrap((ops.flatten :+ copyOp): _*)
    }

  }

  def apply[S: Mangler: Exports: Arrows](
    value: VarModel,
    intoCopy: IntoCopyRaw
  ): State[S, (VarModel, Inline)] = {
    for {
      name <- Mangler[S].findAndForbidName(value.name + "_obj_copy")
      foldedFields <- intoCopy.fields.nonEmptyTraverse(unfold(_))
      varModel = VarModel(name, value.baseType)
      valsInline = foldedFields.toSortedMap.values.map(_._2).fold(Inline.empty)(_ |+| _)
      fields = foldedFields.map(_._1)
      objCopy <- copyObj(value, fields, varModel)
    } yield {
      (
        varModel,
        Inline(
          valsInline.flattenValues,
          Chain.one(SeqModel.wrap((valsInline.predo :+ objCopy).toList: _*)),
          SeqMode
        )
      )
    }

  }
}
