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
import aqua.model.inline.raw.ApplyIntoCopyRawInliner
import aqua.model.inline.state.InliningState
import aqua.raw.ops.*
import aqua.raw.value.*
import aqua.types.*

import cats.data.{Chain, NonEmptyList, NonEmptyMap}
import cats.syntax.show.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CopyInlinerSpec extends AnyFlatSpec with Matchers {

  "copy inliner" should "unfold values in parallel" in {

    val structType = StructType(
      "some_struct",
      NonEmptyMap.of("field1" -> ScalarType.u32, "field2" -> ScalarType.string)
    )

    val arrType = ArrayType(ScalarType.string)
    val length = FunctorRaw("length", ScalarType.u32)
    val lengthValue = VarRaw("l", arrType).withProperty(length)

    val getField = CallServiceRaw(
      LiteralRaw.quote("serv"),
      "get_field",
      ArrowType(NilType, UnlabeledConsType(ScalarType.string, NilType)),
      Nil
    )

    val copyRaw =
      IntoCopyRaw(structType, NonEmptyMap.of("field1" -> lengthValue, "field2" -> getField))
    val varName = "some_struct"
    val varRaw = VarRaw(varName, structType).withProperty(copyRaw)

    val (model, tree) =
      RawValueInliner.valueToModel[InliningState](varRaw, false).run(InliningState()).value._2

    val result = VarModel(varName + "_obj_copy", structType)
    model shouldBe result

    val lengthModel = FunctorModel("length", ScalarType.u32)

    val streamMapName = "some_struct_obj_copy_map"
    val streamMapType = StreamMapType.top()

    tree.get.equalsOrShowDiff(
      SeqModel.wrap(
        ParModel.wrap(
          SeqModel.wrap(
            FlattenModel(VarModel("l", arrType), "l_to_functor").leaf,
            FlattenModel(VarModel("l_to_functor", arrType, Chain.one(lengthModel)), "l_length").leaf
          ),
          CallServiceModel(
            "serv",
            "get_field",
            Nil,
            VarModel("get_field", ScalarType.string)
          ).leaf
        ),
        RestrictionModel(streamMapName, streamMapType).wrap(
          InsertKeyValueModel(
            LiteralModel.quote("field1"),
            VarModel("l_length", ScalarType.u32),
            streamMapName,
            streamMapType
          ).leaf,
          InsertKeyValueModel(
            LiteralModel.quote("field2"),
            VarModel("get_field", ScalarType.string),
            streamMapName,
            streamMapType
          ).leaf,
          CanonicalizeModel(
            VarModel(streamMapName, streamMapType),
            CallModel.Export(result.name, result.`type`)
          ).leaf
        )
      )
    ) shouldBe true

  }

}
