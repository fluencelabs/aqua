package aqua

import cats.data.NonEmptyMap
import org.scalacheck.*
import scala.collection.immutable.SortedMap

package object types {

  val anyName: Gen[String] = for {
    first <- Gen.alphaChar
    other <- Gen.alphaStr
  } yield first + other

  def productOf[T <: Type](gen: Gen[T]): Gen[ProductType] =
    Gen.sized(size => Gen.resize(size / 2, Gen.listOf(gen).map(ProductType.apply)))

  def labeledProductOf[T <: Type](gen: Gen[T]): Gen[ProductType] =
    Gen.sized(size =>
      Gen
        .resize(
          size / 2,
          Gen.listOf(for {
            name <- anyName
            typ <- gen
          } yield name -> typ)
        )
        .map(ProductType.labelled)
    )

  def arrowOf[T <: Type](gen: Gen[T]): Gen[ArrowType] =
    Gen.sized(size =>
      for {
        input <- Gen.resize(size / 2, labeledProductOf(gen))
        output <- Gen.resize(size / 2, productOf(gen))
      } yield ArrowType(input, output)
    )

  given Arbitrary[ScalarType] = Arbitrary(Gen.oneOf(ScalarType.all))

  given Arbitrary[LiteralType] = Arbitrary(
    Gen.oneOf(
      LiteralType.bool,
      LiteralType.unsigned,
      LiteralType.signed,
      LiteralType.float,
      LiteralType.number,
      LiteralType.string
    )
  )

  private def fromData[T](f: DataType => T): Arbitrary[T] =
    Arbitrary(
      Gen.sized(size =>
        Gen
          .resize(
            size / 2,
            Arbitrary.arbitrary[DataType]
          )
          .map(f)
      )
    )

  given Arbitrary[OptionType] =
    fromData(OptionType.apply)

  given Arbitrary[ArrayType] =
    fromData(ArrayType.apply)

  given Arbitrary[CanonStreamType] =
    fromData(CanonStreamType.apply)

  given Arbitrary[StructType] = Arbitrary(
    Gen.sized(size =>
      for {
        name <- anyName
        fields <- Gen
          .nonEmptyMap(
            for {
              name <- anyName
              typ <- Gen.resize(
                size / 2,
                Arbitrary.arbitrary[DataType]
              )
            } yield name -> typ
          )
          .map(m => NonEmptyMap.fromMapUnsafe(SortedMap.from(m)))
      } yield StructType(name, fields)
    )
  )

  given Arbitrary[DataType] = Arbitrary(
    Gen.sized(size =>
      if (size <= 0)
        Gen.oneOf(
          Arbitrary.arbitrary[ScalarType],
          Arbitrary.arbitrary[LiteralType]
        )
      else
        Gen.resize(
          size / 2,
          Gen.oneOf(
            Arbitrary.arbitrary[ScalarType],
            Arbitrary.arbitrary[LiteralType],
            Arbitrary.arbitrary[OptionType],
            Arbitrary.arbitrary[ArrayType],
            Arbitrary.arbitrary[CanonStreamType],
            Arbitrary.arbitrary[StructType]
          )
        )
    )
  )
}
