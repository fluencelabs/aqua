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
import aqua.model.inline.raw.RawInliner
import cats.data.Chain
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{AbilityRaw, LiteralRaw, MakeStructRaw}
import cats.data.{NonEmptyList, NonEmptyMap, State}
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.{unfold, valueToModel}
import aqua.types.{ArrowType, ScalarType}
import cats.syntax.traverse.*
import cats.syntax.monoid.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.apply.*
import cats.syntax.foldable.*

object MakeAbilityRawInliner extends RawInliner[AbilityRaw] {

  override def apply[S: Mangler: Exports: Arrows](
    raw: AbilityRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    for {
      name <- Mangler[S].findAndForbidName(raw.abilityType.name + "_ab")
      foldedFields <- raw.fieldsAndArrows.nonEmptyTraverse(unfold(_))
      varModel = VarModel(name, raw.baseType)
      valsInline = foldedFields.toList.foldMap { case (_, inline) => inline }.desugar
      _ <- foldedFields.toNel.traverse { case (n, (vm, _)) =>
        Exports[S].resolved(s"$name.$n", vm)
      }
    } yield {
      (
        varModel,
        Inline(
          Chain.one(SeqModel.wrap(valsInline.predo))
        )
      )
    }
  }
}
