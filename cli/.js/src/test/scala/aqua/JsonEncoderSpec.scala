package aqua

import aqua.js.JsonEncoder
import aqua.types.{ArrayType, LiteralType, OptionType, StructType}
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.data.{NonEmptyList, NonEmptyMap}

class JsonEncoderSpec extends AnyFlatSpec with Matchers {

  "json encoder" should "get type from json" in {
    val json = scalajs.js.JSON.parse("""{
                                       |"arr2": [{
                                       |      "a": "fef",
                                       |      "b": [1,2,3,4],
                                       |      "c": "erfer"
                                       |    },{
                                       |      "a": "ferfer",
                                       |      "b": [1,2,3,4],
                                       |      "c": "erfer"
                                       |    }, {
                                       |        "a": "as",
                                       |        "d": "gerrt"
                                       |      }]
                                       |}      """.stripMargin)
    val res = JsonEncoder.aquaTypeFromJson("n", json)
    res.isValid shouldBe true

    val elType = StructType(
      "",
      NonEmptyMap.of(
        ("a", LiteralType.string),
        ("b", ArrayType(LiteralType.number)),
        ("c", OptionType(LiteralType.string)),
        ("d", OptionType(LiteralType.string))
      )
    )
    res.toOption.get shouldBe StructType("", NonEmptyMap.of(("arr2", ArrayType(elType))))
  }

  "json encoder" should "get type from json 1" in {
    val json = scalajs.js.JSON.parse("""{
                                       |"arr2": [{
                                       |      "b": [1,2,3,4]
                                       |    },{
                                       |      "b": [1,2,3,4]
                                       |    }, {
                                       |        "b": "gerrt"
                                       |      }]
                                       |}      """.stripMargin)
    val res = JsonEncoder.aquaTypeFromJson("n", json)
    res.isValid shouldBe false
  }

  "json encoder" should "get type from json 2" in {
    val json =
      scalajs.js.JSON.parse(
        """{
          |"arr1": [{"a": [{"c": "", "d": 123}, {"c": ""}], "b": ""}, {"b": ""}],
          |"arr2": [1,2,3,4],
          |"arr3": ["fre", "grt", "rtgrt"],
          |"str": "egrerg",
          |"num": 123
          |}      """.stripMargin
      )
    val res = JsonEncoder.aquaTypeFromJson("n", json)
    res.isValid shouldBe true

    val innerElType = StructType(
      "",
      NonEmptyMap.of(
        ("c", LiteralType.string),
        ("d", OptionType(LiteralType.number))
      )
    )
    val elType = StructType(
      "",
      NonEmptyMap.of(
        ("a", ArrayType(innerElType)),
        ("b", LiteralType.string)
      )
    )

    val t = StructType(
      "",
      NonEmptyMap.of(
        ("arr1", ArrayType(elType)),
        ("arr2", ArrayType(LiteralType.number)),
        ("arr3", ArrayType(LiteralType.string)),
        ("str", LiteralType.string),
        ("num", LiteralType.number)
      )
    )

    res.toOption.get shouldBe t
  }

  "json encoder" should "get type from json 3" in {
    val json = scalajs.js.JSON.parse("""{
                                       |"arr2": [{
                                       |      "b": [1,2,3,4]
                                       |    },{
                                       |      "b": [1,2,3,4]
                                       |    }, {
                                       |        "b": "gerrt"
                                       |      }]
                                       |}      """.stripMargin)
    val res = JsonEncoder.aquaTypeFromJson("n", json)
    res.isValid shouldBe false
  }

  "json encoder" should "get type from json 4" in {
    val json =
      scalajs.js.JSON.parse(
        """{
          |"arr4": [{"a": "", "b": {"c": "", "d": [1,2,3,4]}}, {"a": ""}]
          |}      """.stripMargin
      )
    val res = JsonEncoder.aquaTypeFromJson("n", json)
    res.isValid shouldBe true

    val arr4InnerType = OptionType(
      StructType(
        "",
        NonEmptyMap.of(
          ("c", LiteralType.string),
          ("d", ArrayType(LiteralType.number))
        )
      )
    )

    val arr4ElType = StructType(
      "",
      NonEmptyMap.of(
        ("a", LiteralType.string),
        ("b", arr4InnerType)
      )
    )

    val t = StructType(
      "",
      NonEmptyMap.of(
        ("arr4", ArrayType(arr4ElType))
      )
    )

    res.toOption.get shouldBe t
  }
}
