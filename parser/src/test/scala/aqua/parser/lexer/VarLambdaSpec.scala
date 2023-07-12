package aqua.parser.lexer

import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.Id
import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VarLambdaSpec extends AnyFlatSpec with Matchers with EitherValues {

  import aqua.AquaSpec._

  "var lambda" should "parse" in {
    val opsP = (s: String) => Name.dotted.parseAll(s).value.mapK(spanToId)

    opsP("SomeClass.some_val") should be(Name[Id]("SomeClass.some_val"))

    opsP("some_val") should be(Name[Id]("some_val"))

    opsP("SOME_CONST") should be(Name[Id]("SOME_CONST"))

    opsP("SomeClass.SOME_CONST") should be(Name[Id]("SomeClass.SOME_CONST"))
  }

  "var lambda in VarToken" should "parse" in {
    val opsP = (s: String) => ValueToken.varProperty.parseAll(s).value.mapK(spanToId)

    opsP("some_val") should be(VarToken[Id](Name[Id]("some_val")))

    opsP("SomeClass.SOME_CONST") should be(VarToken[Id](Name[Id]("SomeClass.SOME_CONST")))
  }

  "var lambda in ability" should "parse" in {
    val opsP = (s: String) => ValueToken.abProperty.parseAll(s).value.mapK(spanToId)

    opsP("SomeClass") should be(VarToken[Id](Name[Id]("SomeClass")))

    opsP("SomeClass.call()") should be(
      VarToken[Id](Name[Id]("SomeClass"), IntoArrow(Name[Id]("call"), Nil) :: Nil)
    )
  }

  "parse Class " should "parse" in {
    val opsP = (s: String) => Name.cl.parseAll(s).value.mapK(spanToId)

    opsP("SomeClass") should be(Name[Id]("SomeClass"))

    opsP("SC") should be(Name[Id]("SC"))
  }

}
