/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.model.inline.raw

import aqua.model.ValueModel.Ability
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.state.*
import aqua.model.{SeqModel, ValueModel, VarModel}
import aqua.raw.value.AbilityRaw
import aqua.types.AbilityType

import cats.Eval
import cats.data.{Chain, IndexedStateT, NonEmptyMap, State}
import cats.syntax.foldable.*
import cats.syntax.functor.*

object MakeAbilityRawInliner extends RawInliner[AbilityRaw] {

  private def updateFields[S: Mangler: Exports: Arrows: Config](
    name: String,
    fields: NonEmptyMap[String, (ValueModel, Inline)]
  ): State[S, Unit] =
    fields.toNel.traverse {
      case (n, (Ability(vm, _), _)) =>
        val leftName = AbilityType.fullName(name, n)
        Exports[S].copyWithAbilityPrefix(vm.name, leftName)
      case (n, (vm, _)) =>
        Exports[S].resolveAbilityField(name, n, vm)
    }.as(())

  override def apply[S: Mangler: Exports: Arrows: Config](
    raw: AbilityRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    for {
      name <- Mangler[S].findAndForbidName(raw.abilityType.name + "_anon")
      foldedFields <- raw.fieldsAndArrows.nonEmptyTraverse(unfold(_))
      varModel = VarModel(name, raw.baseType)
      valsInline = foldedFields.toList.foldMap { case (_, inline) => inline }.desugar
      _ <- updateFields(name, foldedFields)
      _ <- Exports[S].resolved(name, varModel)
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
