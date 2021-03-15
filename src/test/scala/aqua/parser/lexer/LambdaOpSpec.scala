package aqua.parser.lexer

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.Id
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.data.NonEmptyList

class LambdaOpSpec extends AnyFlatSpec with Matchers with EitherValues {

  "lambda ops" should "parse" in {
    val opsP = (s: String) => LambdaOp.ops[Id].parseAll(s).right.value

    opsP(".field") should be(NonEmptyList.of(IntoField[Id]("field")))
    opsP(".field.sub") should be(NonEmptyList.of(IntoField[Id]("field"), IntoField[Id]("sub")))
    opsP(".field*.sub") should be(NonEmptyList.of(IntoField[Id]("field"), IntoArray[Id](()), IntoField[Id]("sub")))
  }

}
