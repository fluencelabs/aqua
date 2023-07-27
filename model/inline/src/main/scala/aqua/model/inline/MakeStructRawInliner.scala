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
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{LiteralRaw, MakeStructRaw}
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.{unfold, valueToModel}
import aqua.types.ScalarType

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
    result: VarModel
  ): State[S, OpModel.Tree] = {
    fields.toSortedMap.toList.flatMap { case (name, value) =>
      LiteralModel.fromRaw(LiteralRaw.quote(name)) :: value :: Nil
    }.map(TagInliner.canonicalizeIfStream(_, None)).sequence.map { argsWithOps =>
      val (args, ops) = argsWithOps.unzip
      val createOp =
        CallServiceModel(
          "json",
          "obj",
          args,
          result
        ).leaf
      SeqModel.wrap((ops.flatten :+ createOp): _*)

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
      objCreation <- createObj(fields, varModel)
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
