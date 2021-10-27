package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.FieldTypeExpr
import aqua.types.ScalarType.bool
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FieldTypeExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec._

  "else" should "be parsed" in {
    parseFieldType("some: bool") should be(
      FieldTypeExpr[Id]("some", bool)
    )

    parseFieldType("some: Custom") should be(
      FieldTypeExpr[Id]("some", toCustomType("Custom"))
    )

    parseFieldType("some: []Custom") should be(
      FieldTypeExpr[Id]("some", toArrayType("Custom"))
    )
  }
}
