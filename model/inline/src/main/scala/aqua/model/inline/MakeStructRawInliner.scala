package aqua.model.inline

import aqua.model.*
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.raw.RawInliner
import aqua.model.inline.state.*
import aqua.raw.value.MakeStructRaw
import aqua.types.{StreamMapType, StructType}

import cats.data.{Chain, NonEmptyMap, State}
import cats.syntax.bifunctor.*
import cats.syntax.foldable.*
import cats.syntax.functor.*

object MakeStructRawInliner extends RawInliner[MakeStructRaw] {

  /**
   * Creates structure using stream map.
   * @param mapName stream map name
   * @param mapType stream map type
   * @param result variable with structure
   * @param fields fields to insert
   * @param prefix operations that must be inside stream map restriction
   * @return tree with traversed fields and tree with structure creation.
   *         They are split to combine it later with other trees in a more natural way.
   */
  def constructThroughMap[S: Mangler](
    mapName: String,
    mapType: StreamMapType,
    result: CallModel.Export,
    fields: NonEmptyMap[String, ValueModel],
    prefix: List[OpModel.Tree] = Nil
  ): State[S, OpModel.Tree] = {
    fields.nonEmptyTraverse(TagInliner.canonicalizeIfStream(_)).map { fieldsTraversed =>
      val (values, ops) = fieldsTraversed.toNel.map { case (name, (model, ops)) =>
        (name -> model, ops)
      }.unzip.bimap(_.toList, _.toList.flatten)

      val models = values.map { case (k, v) =>
        InsertKeyValueModel(LiteralModel.quote(k), v, mapName, mapType).leaf
      }

      val toResult =
        CanonicalizeModel(VarModel(mapName, mapType), result).leaf

      RestrictionModel(mapName, mapType).wrap(ops ++ prefix ++ models :+ toResult)
    }
  }

  private def createObj[S: Mangler](
    fields: NonEmptyMap[String, ValueModel],
    resultName: String,
    resultType: StructType
  ): State[S, OpModel.Tree] = {
    val mapType = StreamMapType.top()
    val mapName = resultName + "_map"

    constructThroughMap(mapName, mapType, CallModel.Export(resultName, resultType), fields)
  }

  override def apply[S: Mangler: Exports: Arrows: Config](
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
