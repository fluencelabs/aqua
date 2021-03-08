package aqua.parser.lexer

import aqua.interim.types.LiteralType
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser

class ValueSpec extends AnyFlatSpec with Matchers with EitherValues {

  "var getter" should "parse" in {
    Value.`value`.parseAll("varname").right.value should be(VarLambda("varname", Nil))
    Value.`value`.parseAll("varname.field").right.value should be(VarLambda("varname", IntoField("field") :: Nil))
    Value.`value`.parseAll("varname.field.sub").right.value should be(
      VarLambda("varname", IntoField("field") :: IntoField("sub") :: Nil)
    )
  }

  "literals" should "parse" in {
    Value.`value`.parseAll("true").right.value should be(Literal("true", LiteralType.bool))
    Value.`value`.parseAll("false").right.value should be(Literal("false", LiteralType.bool))

    Value.`value`.parseAll("1").right.value should be(Literal("1", LiteralType.number))
    Value.`value`.parseAll("1111").right.value should be(Literal("1111", LiteralType.number))

    Value.`value`.parseAll("-1543").right.value should be(Literal("-1543", LiteralType.signed))

    Value.`value`.parseAll("1.0").right.value should be(Literal("1.0", LiteralType.float))
    Value.`value`.parseAll("1.23").right.value should be(Literal("1.23", LiteralType.float))
    Value.`value`.parseAll("-1.23").right.value should be(Literal("-1.23", LiteralType.float))

    Value.`value`.parseAll("\"some crazy string\"").right.value should be(
      Literal("\"some crazy string\"", LiteralType.string)
    )
    // This does not work :(
//    Value.`value`.parseAll("\"some crazy string with escaped \\\" quote\"").right.value should be(
//      Literal("\"some crazy string with escaped \\\" quote\"", BasicType.string)
//    )
    Value.`value`.parse("\"just string\"     ").right.value should be(
      ("     ", Literal("\"just string\"", LiteralType.string))
    )
  }

}
