package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.{AliasExpr, DataStructExpr}
import aqua.types.ScalarType.u32
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DataStructExprSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec._

  "data struct" should "be parsed properly" in {
    parseDataStruct("data Smth") should be(
      DataStructExpr[Id]("Smth")
    )

  }
}
