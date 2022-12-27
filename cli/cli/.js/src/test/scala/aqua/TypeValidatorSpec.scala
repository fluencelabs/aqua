package aqua
import aqua.run.TypeValidator
import aqua.types.{ArrayType, LiteralType, OptionType, ScalarType, StructType, Type}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.data.{NonEmptyList, NonEmptyMap, ValidatedNec}

class TypeValidatorSpec extends AnyFlatSpec with Matchers {

  val aquaType = StructType(
    "some",
    NonEmptyMap.of(
      ("field1", OptionType(ArrayType(ScalarType.u8))),
      ("field2", OptionType(ArrayType(OptionType(ScalarType.i32)))),
      ("field3", ArrayType(ArrayType(ArrayType(ScalarType.i64)))),
      (
        "field4",
        OptionType(
          StructType(
            "some2",
            NonEmptyMap.of(
              ("innerfield1", OptionType(ScalarType.u32)),
              ("innerfield2", ArrayType(ScalarType.i16))
            )
          )
        )
      )
    )
  )

  private def validate(aquaType: Type, jsonType: Type) = {
    TypeValidator.validateTypes("some", aquaType, Some(jsonType))
  }

  "type validator" should "return invalid result if check same type" in {
    val res = validate(aquaType, aquaType)
    res.isValid shouldBe false
  }

  "type validator" should "return invalid result if there is no field" in {
    val res = validate(
      StructType(
        "some",
        NonEmptyMap.of(
          ("field1", ScalarType.u8),
          ("field2", ArrayType(ScalarType.string))
        )
      ),
      StructType(
        "some",
        NonEmptyMap.of(
          ("field1", ScalarType.u8)
        )
      ))
    res.isValid shouldBe false
  }

  "type validator" should "validate optional types properly" in {
    val aquaOptionalArrArrType = OptionType(ArrayType(ArrayType(ScalarType.u8)))
    val aquaOptionalArrType = OptionType(ArrayType(ScalarType.u8))
    val aquaOptionalType = OptionType(ScalarType.u8)

    val res1 = validate(aquaOptionalType, LiteralType.number)
    res1.isValid shouldBe true
    val res2 = validate(aquaOptionalArrType, ArrayType(LiteralType.number))
    res2.isValid shouldBe true
    val res3 = validate(aquaOptionalArrArrType, ArrayType(ArrayType(LiteralType.number)))
    res3.isValid shouldBe true

    val res1Invalid = validate(aquaOptionalType, ArrayType(LiteralType.number))
    res1Invalid.isValid shouldBe false
  }

  "type validator" should "validate array types properly" in {
    val aquaArrArrArrType = ArrayType(ArrayType(ArrayType(ScalarType.u8)))
    val aquaArrArrType = ArrayType(ArrayType(ScalarType.u8))
    val aquaArrType = ArrayType(ScalarType.u8)

    val res1 = validate(aquaArrType, ArrayType(LiteralType.number))
    res1.isValid shouldBe true
    val res2 = validate(aquaArrArrType, ArrayType(ArrayType(LiteralType.number)))
    res2.isValid shouldBe true
    val res3 = validate(aquaArrArrArrType, ArrayType(ArrayType(ArrayType(LiteralType.number))))
    res3.isValid shouldBe true

    val res1invalid = validate(aquaArrType, LiteralType.number)
    res1invalid.isInvalid shouldBe true
    val res2invalid = validate(aquaArrArrType, ArrayType(LiteralType.number))
    res2invalid.isInvalid shouldBe true
  }

  "type validator" should "validate options with arrays types properly" in {
    val aquaOptArrOptArrType = OptionType(ArrayType(OptionType(ArrayType(ScalarType.u8))))

    val res1 = validate(aquaOptArrOptArrType, ArrayType(ArrayType(LiteralType.number)))
    res1.isValid shouldBe true

    val res1invalid =
      validate(aquaOptArrOptArrType, ArrayType(ArrayType(ArrayType(LiteralType.number))))
    res1invalid.isValid shouldBe false
    val res2invalid =
      validate(aquaOptArrOptArrType, ArrayType(ArrayType(ArrayType(ArrayType(LiteralType.number)))))
    res2invalid.isValid shouldBe false
  }

  "type validator" should "validate complex types properly" in {

    val res1 = validate(
      aquaType,
      StructType(
        "some",
        NonEmptyMap.of(
          ("field1", ArrayType(LiteralType.number)),
          ("field2", ArrayType(LiteralType.number)),
          ("field3", ArrayType(ArrayType(ArrayType(LiteralType.number)))),
          (
            "field4",
            StructType(
              "some2",
              NonEmptyMap.of(
                ("innerfield1", LiteralType.number),
                ("innerfield2", ArrayType(LiteralType.number))
              )
            )
          )
        )
      )
    )

    res1.isValid shouldBe true
  }

  "type validator" should "return invalid if there is no field" in {
    val structType = StructType(
      "some",
      NonEmptyMap.of(
        ("field1", ScalarType.u8),
        ("field2", ScalarType.string),
        ("field3", OptionType(ScalarType.string))
      )
    )

    val res1invalid = validate(
      structType,
      StructType(
        "some",
        NonEmptyMap.of(
          ("field2", LiteralType.string)
        )
      )
    )
    res1invalid.isValid shouldBe false

    val res2invalid = validate(
      structType,
      StructType(
        "some",
        NonEmptyMap.of(
          ("field1", ScalarType.u8)
        )
      )
    )
    res2invalid.isValid shouldBe false

    val res1 = validate(
      structType,
      StructType(
        "some",
        NonEmptyMap.of(
          ("field1", LiteralType.number),
          ("field2", LiteralType.string)
        )
      )
    )
    res1.isValid shouldBe true

    validate(
      structType,
      StructType(
        "some",
        NonEmptyMap.of(
          ("field1", ScalarType.u8),
          ("field2", ScalarType.string),
          ("field3", ScalarType.string)
        )
      )
    ).isValid shouldBe true
  }

  "type validator" should "return invalid if there is one array when it must be two" in {
    val leftType = StructType(
      "some",
      NonEmptyMap.of(
        ("arrr", OptionType(ArrayType(ArrayType(ScalarType.u8))))
      )
    )

    val rightType = StructType(
      "some",
      NonEmptyMap.of(
        ("arrr", ArrayType(LiteralType.number))
      )
    )

    validate(leftType, rightType).isInvalid shouldBe true
  }
}
