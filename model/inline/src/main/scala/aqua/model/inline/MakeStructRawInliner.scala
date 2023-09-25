package aqua.model.inline

import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.raw.RawInliner
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.model.*
import aqua.raw.value.MakeStructRaw
import aqua.types.{StreamMapType, StructType}

import cats.data.{Chain, NonEmptyMap, State}
import cats.syntax.foldable.*
import cats.syntax.bifunctor.*
import cats.syntax.functor.*

object MakeStructRawInliner extends RawInliner[MakeStructRaw] {

  /**
   * Creates structure using stream map.
   * @param mapName stream map name
   * @param mapType stream map name
   * @param result variable with structure
   * @param fields fields to insert
   * @return value with assembled structure
   */
  def constructThroughMap[S: Mangler](
    mapName: String,
    mapType: StreamMapType,
    result: CallModel.Export,
    fields: NonEmptyMap[String, ValueModel]
  ): State[S, (List[OpModel.Tree], List[OpModel.Tree])] = {
    fields.nonEmptyTraverse(TagInliner.canonicalizeIfStream(_)).map { fieldsTraversed =>
      val (values, ops) = fieldsTraversed.toNel.map { case (name, (model, ops)) =>
        (name -> model, ops)
      }.unzip.bimap(_.toList, _.toList.flatten)

      val models = values.map { case (k, v) =>
        InsertKeyValueModel(LiteralModel.quote(k), v, mapName, mapType).leaf
      }

      val toResult =
        CanonicalizeModel(VarModel(mapName, mapType), result).leaf

      (ops, models :+ toResult)
    }
  }

  private def createObj[S: Mangler](
    fields: NonEmptyMap[String, ValueModel],
    resultName: String,
    resultType: StructType
  ): State[S, OpModel.Tree] = {
    val mapType = StreamMapType.top()
    val mapName = resultName + "_map"

    constructThroughMap(mapName, mapType, CallModel.Export(resultName, resultType), fields).map(r =>
      SeqModel.wrap(r._1 ++ r._2)
    )
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
