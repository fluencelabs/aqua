package aqua.parser.lexer

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.semantics.LiteralType
import cats.Id

class ValueSpec extends AnyFlatSpec with Matchers with EitherValues {

  "var getter" should "parse" in {
    Value.`value`.parseAll("varname").right.value should be(VarLambda(Name[Id]("varname"), Nil))
    Value.`value`.parseAll("varname.field").right.value should be(
      VarLambda(Name[Id]("varname"), IntoField[Id]("field") :: Nil)
    )
    Value.`value`.parseAll("varname.field.sub").right.value should be(
      VarLambda(Name[Id]("varname"), IntoField[Id]("field") :: IntoField[Id]("sub") :: Nil)
    )
  }

  "literals" should "parse" in {
    Value.`value`.parseAll("true").right.value should be(Literal[Id]("true", LiteralType.bool))
    Value.`value`.parseAll("false").right.value should be(Literal[Id]("false", LiteralType.bool))

    Value.`value`.parseAll("1").right.value should be(Literal[Id]("1", LiteralType.number))
    Value.`value`.parseAll("1111").right.value should be(Literal[Id]("1111", LiteralType.number))

    Value.`value`.parseAll("-1543").right.value should be(Literal[Id]("-1543", LiteralType.signed))

    Value.`value`.parseAll("1.0").right.value should be(Literal[Id]("1.0", LiteralType.float))
    Value.`value`.parseAll("1.23").right.value should be(Literal[Id]("1.23", LiteralType.float))
    Value.`value`.parseAll("-1.23").right.value should be(Literal[Id]("-1.23", LiteralType.float))

    Value.`value`.parseAll("\"some crazy string\"").right.value should be(
      Literal[Id]("\"some crazy string\"", LiteralType.string)
    )
    // This does not work :(
//    Value.`value`.parseAll("\"some crazy string with escaped \\\" quote\"").right.value should be(
//      Literal("\"some crazy string with escaped \\\" quote\"", BasicType.string)
//    )
    Value.`value`.parse("\"just string\"     ").right.value should be(
      ("     ", Literal[Id]("\"just string\"", LiteralType.string))
    )
  }

}
