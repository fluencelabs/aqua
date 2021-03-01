package aqua.parser.lexer

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ValueSpec extends AnyFlatSpec with Matchers with EitherValues {

  "var getter" should "parse" in {
    Value.`value`.parseAll("varname").right.value should be(VarLambda("varname", None))
    Value.`value`.parseAll("varname.field").right.value should be(VarLambda("varname", Some("field")))
    Value.`value`.parseAll("varname.field.sub").right.value should be(VarLambda("varname", Some("field.sub")))
  }

  "literals" should "parse" in {
    Value.`value`.parseAll("true").right.value should be(Literal("true", BasicType.bool))
    Value.`value`.parseAll("false").right.value should be(Literal("false", BasicType.bool))

    Value.`value`.parseAll("1").right.value should be(Literal("1", BasicType.number))
    Value.`value`.parseAll("1111").right.value should be(Literal("1111", BasicType.number))

    Value.`value`.parseAll("-1543").right.value should be(Literal("-1543", BasicType.signed))

    Value.`value`.parseAll("1.0").right.value should be(Literal("1.0", BasicType.float))
    Value.`value`.parseAll("1.23").right.value should be(Literal("1.23", BasicType.float))
    Value.`value`.parseAll("-1.23").right.value should be(Literal("-1.23", BasicType.float))

    Value.`value`.parseAll("\"some crazy string\"").right.value should be(
      Literal("\"some crazy string\"", BasicType.string)
    )
    // This does not work :(
//    Value.`value`.parseAll("\"some crazy string with escaped \\\" quote\"").right.value should be(
//      Literal("\"some crazy string with escaped \\\" quote\"", BasicType.string)
//    )
    Value.`value`.parse("\"just string\"     ").right.value should be(
      ("     ", Literal("\"just string\"", BasicType.string))
    )
  }

}
