package aqua.model.inline.raw

import aqua.model.ValueModel.Ability
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.model.{SeqModel, ValueModel, VarModel}
import aqua.raw.value.AbilityRaw
import aqua.types.AbilityType

import cats.Eval
import cats.data.{Chain, IndexedStateT, NonEmptyMap, State}
import cats.syntax.foldable.*
import cats.syntax.functor.*

object MakeAbilityRawInliner extends RawInliner[AbilityRaw] {

  private def updateFields[S: Mangler: Exports: Arrows](
    name: String,
    fields: NonEmptyMap[String, (ValueModel, Inline)]
  ): State[S, Unit] =
    fields.toNel.traverse {
      case (n, (Ability(abilityName, _, _), _)) =>
        val leftName = AbilityType.fullName(name, n)
        Exports[S].copyWithAbilityPrefix(abilityName, leftName)
      case (n, (vm, _)) =>
        Exports[S].resolveAbilityField(name, n, vm)
    }.as(())

  override def apply[S: Mangler: Exports: Arrows](
    raw: AbilityRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    for {
      name <- Mangler[S].findAndForbidName(raw.abilityType.name + "_anon")
      foldedFields <- raw.fieldsAndArrows.nonEmptyTraverse(unfold(_))
      varModel = VarModel(name, raw.baseType)
      valsInline = foldedFields.toList.foldMap { case (_, inline) => inline }.desugar
      _ <- updateFields(name, foldedFields)
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
