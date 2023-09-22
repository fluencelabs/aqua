package aqua.model.inline

import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.raw.RawInliner
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.model.*
import aqua.raw.value.MakeStructRaw
import aqua.types.{StreamMapType, StructType}

import cats.data.{Chain, NonEmptyMap, State}
import cats.syntax.foldable.*
import cats.syntax.functor.*

object MakeStructRawInliner extends RawInliner[MakeStructRaw] {

  private def createObj[S: Mangler](
    fields: NonEmptyMap[String, ValueModel],
    resultName: String,
    resultType: StructType
  ): State[S, OpModel.Tree] = {
    fields.nonEmptyTraverse(TagInliner.canonicalizeIfStream(_)).map { fieldsTraversed =>
      val mapType = StreamMapType.fromStruct(resultType)
      val mapVar = VarModel(resultName + "_map", mapType)
      val (values, ops) = fieldsTraversed.toNel.map { case (name, (model, ops)) =>
        (name -> model, ops)
      }.unzip.bimap(_.toList, _.toList.flatten)
      val models = values.map { case (k, v) =>
        InsertKeyValueModel(LiteralModel.quote(k), v, mapVar.name, mapType).leaf
      }.toList

      val toResult = CanonicalizeModel(mapVar, CallModel.Export(resultName, resultType)).leaf

      SeqModel.wrap(ops ++ models :+ toResult)
    }
  }

  override def apply[S: Mangler: Exports: Arrows](
    raw: MakeStructRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    for {
      name <- Mangler[S].findAndForbidName(raw.structType.name + "_obj")
      foldedFields <- raw.fields.nonEmptyTraverse(unfold(_))
      varModel = VarModel(name, raw.baseType)
      valsInline = foldedFields.foldMap { case (_, inline) => inline }.desugar
      fields = foldedFields.map { case (vm, _) => vm }
      objCreation <- createObj(fields, name, raw.structType)
    } yield {
      (
        varModel,
        Inline(
          Chain.one(SeqModel.wrap(valsInline.predo :+ objCreation))
        )
      )
    }
  }
}
