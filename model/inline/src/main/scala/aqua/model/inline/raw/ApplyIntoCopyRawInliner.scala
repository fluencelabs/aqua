package aqua.model.inline.raw

import aqua.model.inline.Inline.MergeMode.*
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.model.inline.{Inline, TagInliner}
import aqua.model.*
import aqua.raw.value.IntoCopyRaw
import aqua.types.{StreamMapType, StructType}
import cats.data.{Chain, NonEmptyMap, State}
import cats.syntax.foldable.*
import cats.syntax.functor.*
import scribe.Logging

object ApplyIntoCopyRawInliner extends Logging {

  private def copyObj[S: Mangler](
    oldValue: VarModel,
    resultName: String,
    resultType: StructType,
    fields: NonEmptyMap[String, ValueModel]
  ): State[S, OpModel.Tree] = {
    val mapType = StreamMapType.fromStruct(resultType)
    val mapVar = VarModel(resultName + "_map", mapType)

    val nonCopiedValues = resultType.fields.toNel.filterNot { case (k, _) =>
      fields.contains(k)
    }.map { case (k, t) =>
      InsertKeyValueModel(
        LiteralModel.quote(k),
        oldValue.copy(properties = Chain.one(IntoFieldModel(k, t))),
        mapVar.name,
        mapType
      ).leaf
    }
    fields.nonEmptyTraverse(TagInliner.canonicalizeIfStream(_)).map { fieldsTraversed =>
      val ops = fieldsTraversed.toNel.toList.flatMap(_._2._2)
      val models = fieldsTraversed.toNel.map { case (k, v) =>
        InsertKeyValueModel(LiteralModel.quote(k), v._1, mapVar.name, mapType).leaf
      }.toList

      val toResult = CanonicalizeModel(mapVar, CallModel.Export(resultName, resultType)).leaf

      SeqModel.wrap(ops ++ nonCopiedValues ++ models :+ toResult)
    }

  }

  def apply[S: Mangler: Exports: Arrows](
    value: VarModel,
    intoCopy: IntoCopyRaw
  ): State[S, (VarModel, Inline)] = {
    value.`type` match {
      case st: StructType =>
        for {
          resultName <- Mangler[S].findAndForbidName(st.name + "_obj_copy")
          foldedFields <- intoCopy.fields.nonEmptyTraverse(unfold(_))
          varModel = VarModel(resultName, st)
          valsInline = foldedFields.toList.foldMap { case (_, inline) => inline }.desugar
          fields = foldedFields.map(_._1)
          objCopy <- copyObj(value, resultName, st, fields)
        } yield {
          (
            varModel,
            Inline(
              Chain.one(SeqModel.wrap(valsInline.predo :+ objCopy)),
              SeqMode
            )
          )
        }
      case _ =>
        logger.error("Unreachable. Cannot copy a value that is not a data type")
        State.pure((value, Inline.empty))
    }

  }
}
