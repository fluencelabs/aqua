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

import aqua.model.*
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.{unfold, valueToModel}
import aqua.model.inline.raw.RawInliner
import aqua.model.inline.state.*
import aqua.raw.value.ApplyUnaryOpRaw
import aqua.raw.value.ApplyUnaryOpRaw.Op.*
import aqua.raw.value.{AbilityRaw, LiteralRaw, MakeStructRaw}
import aqua.types.{ArrowType, ScalarType}

import cats.data.Chain
import cats.data.{NonEmptyList, NonEmptyMap, State}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.monoid.*
import cats.syntax.traverse.*

object ApplyUnaryOpRawInliner extends RawInliner[ApplyUnaryOpRaw] {

  override def apply[S: Mangler: Exports: Arrows: Config](
    raw: ApplyUnaryOpRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = for {
    value <- unfold(raw.value)
    (vm, vinline) = value

    result <- vm match {
      // Optimize in case of value is known at compile time
      case LiteralModel.Bool(bvalue) =>
        (raw.op match {
          case Not => (LiteralModel.bool(!bvalue), vinline)
        }).pure[State[S, *]]
      // Produce unoptimized inline
      case _ => fullInline(vm, vinline, raw.op)
    }
  } yield result

  private def fullInline[S: Mangler: Exports: Arrows: Config](
    vm: ValueModel,
    vinline: Inline,
    op: ApplyUnaryOpRaw.Op
  ): State[S, (ValueModel, Inline)] = {
    val name = op match {
      case Not => "not"
    }

    /*
     * (seq
     *   <value-inline>
     *   (xor
     *     (match <value-res> true
     *       (ap false <res-name>)
     *     )
     *     (ap true <res-name>)
     *   )
     * )
     */
    val predo = (resName: String) =>
      SeqModel.wrap(
        vinline.predo :+ XorModel.wrap(
          MatchMismatchModel(
            vm,
            LiteralModel.bool(true),
            shouldMatch = true
          ).wrap(
            FlattenModel(
              LiteralModel.bool(false),
              resName
            ).leaf
          ),
          FlattenModel(
            LiteralModel.bool(true),
            resName
          ).leaf
        )
      )

    Mangler[S]
      .findAndForbidName(name)
      .map(resName =>
        (
          VarModel(resName, ScalarType.bool),
          Inline(Chain.one(predo(resName)))
        )
      )
  }
}
