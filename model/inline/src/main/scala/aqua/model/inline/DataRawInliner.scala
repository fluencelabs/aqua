package aqua.model.inline

import aqua.model.{
  CallModel,
  CallServiceModel,
  LiteralModel,
  OpModel,
  SeqModel,
  ValueModel,
  VarModel
}
import aqua.model.inline.raw.RawInliner
import cats.data.Chain
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{MakeStructRaw, LiteralRaw}
import cats.data.{NonEmptyMap, State}
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.{unfold, valueToModel}
import aqua.types.ScalarType
import cats.syntax.traverse.*
import cats.syntax.monoid.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.apply.*

object DataRawInliner extends RawInliner[MakeStructRaw] {

  private def createObj(fields: NonEmptyMap[String, ValueModel], result: VarModel): OpModel.Tree = {
    val args = fields.toSortedMap.toList.flatMap { case (name, value) =>
      LiteralModel.fromRaw(LiteralRaw.quote(name)) :: value :: Nil
    }
    CallServiceModel(
      LiteralModel("\"json\"", ScalarType.string),
      "obj",
      CallModel(
        args,
        CallModel.Export(result.name, result.`type`) :: Nil
      )
    ).leaf
  }

  override def apply[S: Mangler: Exports: Arrows](
    raw: MakeStructRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    for {
      name <- Mangler[S].findAndForbidName(raw.name + "_obj")
      foldedFields <- raw.fields.nonEmptyTraverse(unfold(_))
    } yield {
      val varModel = VarModel(name, raw.baseType)
      val valsInline = foldedFields.toSortedMap.values.map(_._2).fold(Inline.empty)(_ |+| _)
      val fields = foldedFields.map(_._1)
      val objCreation = createObj(fields, varModel)
      (
        varModel,
        Inline(
          valsInline.flattenValues,
          Chain.one(SeqModel.wrap((valsInline.predo :+ objCreation).toList: _*))
        )
      )
    }
  }
}
