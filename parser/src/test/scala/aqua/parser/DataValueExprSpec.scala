package aqua.parser

import aqua.AquaSpec
import aqua.AquaSpec.{toNumber, toStr, toVar}
import aqua.parser.expr.ConstantExpr
import aqua.parser.expr.func.AssignmentExpr
import aqua.parser.lexer.CollectionToken.Mode.ArrayMode
import aqua.parser.lexer.{Ability, CallArrowToken, CollectionToken, DataValueToken, LiteralToken, Name, VarToken}
import aqua.types.LiteralType
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DataValueExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec._

  "data value" should "be parsed" in {
    val one = LiteralToken[Id]("1", LiteralType.number)
    val two = LiteralToken[Id]("2", LiteralType.number)
    val three = LiteralToken[Id]("3", LiteralType.number)
    val a = LiteralToken[Id]("\"a\"", LiteralType.string)
    val b = LiteralToken[Id]("\"b\"", LiteralType.string)
    val c = LiteralToken[Id]("\"c\"", LiteralType.string)

    parseData(
      """Obj(1, "a", [1,2,3], ["b", "c"], NestedObj(2, "b", funcCall(3), value), funcCall(1), Serv.call(2))"""
    ) should be(
      DataValueToken(
        Name[Id]("Obj"),
        List(
          one,
          a,
          CollectionToken(ArrayMode, List(one, two, three)),
          CollectionToken(ArrayMode, List(b, c)),
          DataValueToken(
            Name[Id]("NestedObj"),
            List(two, b, CallArrowToken(None, Name("funcCall"), List(three)), VarToken[Id](Name[Id]("value"), Nil)),
          ),
          CallArrowToken(None, Name("funcCall"), List(one)),
          CallArrowToken(Option(Ability("Serv")), Name("call"), List(two))
        )
      )
    )
  }
}