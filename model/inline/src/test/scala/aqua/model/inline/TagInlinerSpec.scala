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

package aqua.model.inline

import aqua.model.*
import aqua.model.inline.TagInliner.TagInlined
import aqua.model.inline.state.Exports.ExportsState
import aqua.model.inline.state.InliningState
import aqua.raw.ops.{Call, CanonicalizeTag, FlattenTag, ForTag}
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{ScalarType, StreamType}

import cats.data.{Chain, State}
import cats.syntax.show.*
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TagInlinerSpec extends AnyFlatSpec with Matchers with Inside {

  "CanonicalizeTag" should "pass literals as is" in {
    val canonTo = "canon_to"

    val (state, inlined) = TagInliner
      .tagToModel[InliningState](
        CanonicalizeTag(ValueRaw.Nil, Call.Export(canonTo, StreamType(ScalarType.string)))
      )
      .run(InliningState())
      .value

    state.resolvedExports.values(canonTo) shouldBe LiteralModel(
      ValueRaw.Nil.value,
      ValueRaw.Nil.baseType
    )

    inside(inlined) { case TagInlined.Empty(prefix) =>
      prefix shouldBe None
    }
  }

  "FlattenTag" should "pass literals as is" in {
    val canonTo = "canon_to"

    val (state, inlined) = TagInliner
      .tagToModel[InliningState](
        FlattenTag(ValueRaw.Nil, canonTo)
      )
      .run(InliningState())
      .value

    state.resolvedExports.values(canonTo) shouldBe LiteralModel(
      ValueRaw.Nil.value,
      ValueRaw.Nil.baseType
    )

    inside(inlined) { case TagInlined.Empty(prefix) =>
      prefix shouldBe None
    }
  }

  "ForTag" should "not canonicalize iterable in RecMode" in {
    val iterableRaw = VarRaw("iterable", StreamType(ScalarType.string))
    val iterableModel = ValueModel.fromRaw(iterableRaw)

    val tag = ForTag("i", iterableRaw, ForTag.Mode.RecMode)

    val (state, inlined) = TagInliner
      .tagToModel[InliningState](tag)
      .run(
        InliningState(
          resolvedExports = ExportsState(
            Map(
              iterableRaw.name -> iterableModel
            )
          )
        )
      )
      .value

    inside(inlined) { case TagInlined.Around(st, _) =>
      inside(st(Chain.empty).runA(state).value.head) {
        case ForModel(_, iter, ForModel.Mode.Never) =>
          iter shouldBe iterableModel
      }
    }
  }
}
