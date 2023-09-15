package aqua.model.inline

import aqua.model.{CallModel, CallServiceModel, CanonicalizeModel, FlattenModel, InsertKeyValueModel, LiteralModel, OpModel, SeqModel, ValueModel, VarModel}
import aqua.model.inline.raw.RawInliner
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{LiteralRaw, MakeStructRaw}
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.{unfold, valueToModel}
import aqua.types.{ScalarType, StructType, TopType}
import cats.data.Chain
import cats.data.{NonEmptyMap, State}
import cats.syntax.traverse.*
import cats.syntax.monoid.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.apply.*
import cats.syntax.foldable.*

object MakeStructRawInliner extends RawInliner[MakeStructRaw] {

  private def createObj[S: Mangler](
    fields: NonEmptyMap[String, ValueModel],
    resultName: String,
    resultType: StructType
  ): State[S, OpModel.Tree] = {
    fields.nonEmptyTraverse(TagInliner.canonicalizeIfStream(_)).map { kv =>
      val mapName = "%" + resultName + "_map"
      val ops = kv.toNel.toList.flatMap(_._2._2)
      val models = kv.toNel.map { case (k, v) =>
        InsertKeyValueModel(LiteralModel.quote(k), v._1, mapName, resultType).leaf
      }.toList

      val toResult = CanonicalizeModel(VarModel(mapName, TopType), CallModel.Export(resultName, resultType)).leaf

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
