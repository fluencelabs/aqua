package aqua.parser

import aqua.Utils
import aqua.parser.expr.ArrowTypeExpr
import aqua.semantics.ScalarType.string
import aqua.semantics.ScalarType.u32
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArrowTypeExprSpec extends AnyFlatSpec with Matchers with Utils {

  import Utils._

  "arrow types" should "be parsed properly" in {
    parseArrow("onIn: string -> ()") should be(
      ArrowTypeExpr[Id]("onIn", toArrowType(List(string), None))
    )

    parseArrow("onIn: Custom -> Custom2") should be(
      ArrowTypeExpr[Id]("onIn", toArrowType(List("Custom"), Some("Custom2")))
    )

    parseArrow("onIn: Custom, string, u32, Custom3 -> Custom2") should be(
      ArrowTypeExpr[Id]("onIn", toArrowType(List("Custom", string, u32, "Custom3"), Some("Custom2")))
    )
  }
}
