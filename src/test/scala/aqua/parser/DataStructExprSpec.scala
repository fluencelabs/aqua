package aqua.parser

import aqua.Utils
import aqua.parser.expr.{AliasExpr, DataStructExpr}
import aqua.semantics.ScalarType.u32
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DataStructExprSpec extends AnyFlatSpec with Matchers with Utils {

  import Utils._

  "data struct" should "be parsed properly" in {
    parseDataStruct("data Smth") should be(
      DataStructExpr[Id]("Smth")
    )

  }
}
