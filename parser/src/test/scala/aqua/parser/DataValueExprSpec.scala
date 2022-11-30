package aqua.parser

import aqua.AquaSpec
import aqua.AquaSpec.{toNumber, toStr, toVar}
import aqua.parser.expr.ConstantExpr
import aqua.parser.expr.func.AssignmentExpr
import aqua.parser.lexer.CollectionToken.Mode.ArrayMode
import aqua.parser.lexer.{
  Ability,
  CallArrowToken,
  CollectionToken,
  CustomTypeToken,
  StructValueToken,
  LiteralToken,
  Name,
  VarToken
}
import aqua.types.LiteralType
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.data.NonEmptyMap

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
      """Obj(f1 = 1, f2 = "a", f3 = [1,2,3], f4=["b", "c"], f5 =NestedObj(i1 = 2, i2 = "b", i3= funcCall(3), i4 = value), f6=funcCall(1), f7 = Serv.call(2))"""
    ) should be(
      StructValueToken(
        CustomTypeToken[Id]("Obj"),
        NonEmptyMap.of(
          "f1" -> one,
          "f2" -> a,
          "f3" -> CollectionToken[Id](ArrayMode, List(one, two, three)),
          "f4" -> CollectionToken[Id](ArrayMode, List(b, c)),
          "f5" -> StructValueToken(
            CustomTypeToken[Id]("NestedObj"),
            NonEmptyMap.of(
              "i1" -> two,
              "i2" -> b,
              "i3" -> CallArrowToken(None, Name[Id]("funcCall"), List(three)),
              "i4" -> VarToken[Id](Name[Id]("value"), Nil)
            )
          ),
          "f6" -> CallArrowToken(None, Name[Id]("funcCall"), List(one)),
          "f7" -> CallArrowToken(Option(Ability[Id]("Serv")), Name[Id]("call"), List(two))
        )
      )
    )
  }
}
