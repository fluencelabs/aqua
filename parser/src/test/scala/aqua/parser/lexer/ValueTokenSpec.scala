package aqua.parser.lexer

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.types.LiteralType
import cats.Id

class ValueTokenSpec extends AnyFlatSpec with Matchers with EitherValues {

  import aqua.AquaSpec._

  "var getter" should "parse" in {
    ValueToken.`value`.parseAll("varname").value.mapK(spanToId) should be(VarToken(Name[Id]("varname"), Nil))
    ValueToken.`value`.parseAll("varname.field").value.mapK(spanToId) should be(
      VarToken(Name[Id]("varname"), IntoField[Id]("field") :: Nil)
    )
    ValueToken.`value`.parseAll("varname.field.sub").value.mapK(spanToId) should be(
      VarToken(Name[Id]("varname"), IntoField[Id]("field") :: IntoField[Id]("sub") :: Nil)
    )
  }

  "literals" should "parse" in {
    ValueToken.`value`.parseAll("true").value.mapK(spanToId) should be(LiteralToken[Id]("true", LiteralType.bool))
    ValueToken.`value`.parseAll("false").value.mapK(spanToId) should be(LiteralToken[Id]("false", LiteralType.bool))

    ValueToken.`value`.parseAll("1").value.mapK(spanToId) should be(LiteralToken[Id]("1", LiteralType.number))
    ValueToken.`value`.parseAll("1111").value.mapK(spanToId) should be(LiteralToken[Id]("1111", LiteralType.number))

    ValueToken.`value`.parseAll("-1543").value.mapK(spanToId) should be(LiteralToken[Id]("-1543", LiteralType.signed))

    ValueToken.`value`.parseAll("1.0").value.mapK(spanToId) should be(LiteralToken[Id]("1.0", LiteralType.float))
    ValueToken.`value`.parseAll("1.23").value.mapK(spanToId) should be(LiteralToken[Id]("1.23", LiteralType.float))
    ValueToken.`value`.parseAll("-1.23").value.mapK(spanToId) should be(LiteralToken[Id]("-1.23", LiteralType.float))

    ValueToken.`value`.parseAll("\"some crazy string\"").value.mapK(spanToId) should be(
      LiteralToken[Id]("\"some crazy string\"", LiteralType.string)
    )
    // This does not work :(
//    Value.`value`.parseAll("\"some crazy string with escaped \\\" quote\"").value.mapK(spanToId) should be(
//      Literal("\"some crazy string with escaped \\\" quote\"", BasicType.string)
//    )
    val res = ValueToken.`value`.parse("\"just string\"     ").value
    (res._1, res._2.mapK(spanToId)) should be(
      ("     ", LiteralToken[Id]("\"just string\"", LiteralType.string))
    )
  }

}
