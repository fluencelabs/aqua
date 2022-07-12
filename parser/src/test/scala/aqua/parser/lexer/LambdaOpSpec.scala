package aqua.parser.lexer

import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.Id
import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LambdaOpSpec extends AnyFlatSpec with Matchers with EitherValues {

  import aqua.AquaSpec._

  "lambda ops" should "parse" in {
    val opsP = (s: String) => LambdaOp.ops.parseAll(s).value.map(_.mapK(spanToId))

    opsP(".field") should be(NonEmptyList.of(IntoField[Id]("field")))
    opsP(".field.sub") should be(NonEmptyList.of(IntoField[Id]("field"), IntoField[Id]("sub")))

    LambdaOp.ops.parseAll("[-1]").isLeft shouldBe true
    LambdaOp.ops.parseAll("!-1").isLeft shouldBe true

  }

}
