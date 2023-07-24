package aqua.parser

import aqua.AquaSpec
import aqua.AquaSpec.{toNumber, toStr, toVar}
import aqua.parser.expr.ConstantExpr
import aqua.parser.expr.func.AssignmentExpr
import aqua.parser.lexer.{
  Ability,
  CallArrowToken,
  CollectionToken,
  IntoArrow,
  LiteralToken,
  Name,
  NamedTypeToken,
  NamedValueToken,
  Token,
  ValueToken,
  VarToken
}
import aqua.parser.lexer.CollectionToken.Mode.ArrayMode
import aqua.types.LiteralType
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.data.{NonEmptyList, NonEmptyMap}

class StructValueExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec._

  private def parseAndCheckStruct(str: String) = {

    val one = toNumber(1)
    val two = toNumber(2)
    val three = toNumber(3)
    val a = LiteralToken[Id]("\"a\"", LiteralType.string)
    val b = LiteralToken[Id]("\"b\"", LiteralType.string)
    val c = LiteralToken[Id]("\"c\"", LiteralType.string)

    parseData(
      str
    ) should be(
      NamedValueToken(
        NamedTypeToken[Id]("Obj"),
        NonEmptyMap.of(
          "f1" -> one,
          "f2" -> a,
          "f3" -> CollectionToken[Id](ArrayMode, List(one, two, three)),
          "f4" -> CollectionToken[Id](ArrayMode, List(b, c)),
          "f5" -> NamedValueToken(
            NamedTypeToken[Id]("NestedObj"),
            NonEmptyMap.of(
              "i1" -> two,
              "i2" -> b,
              "i3" -> CallArrowToken(None, Name[Id]("funcCall"), List(three)),
              "i4" -> VarToken[Id](Name[Id]("value"), Nil)
            )
          ),
          "f6" -> CallArrowToken(None, Name[Id]("funcCall"), List(one)),
          "f7" -> CallArrowToken(Option(NamedTypeToken[Id]("Serv")), Name[Id]("call"), List(two))
        )
      )
    )
  }

  "one named arg" should "be parsed" in {
    val result = aqua.parser.lexer.Token.namedArg
      .parseAll("""  a
                  | =
                  |  3""".stripMargin)
      .map(v => (v._1, v._2.mapK(spanToId)))
      .value

    result should be(("a", toNumber(3)))
  }

  "named args" should "be parsed" in {
    val result = Token.namedArgs
      .parseAll("""(
                  |a = "str",
                  |b = 3,
                  |c
                  |  =
                  |    5
                  |)""".stripMargin)
      .value
      .map { case (str, vt) => (str, vt.mapK(spanToId)) }

    result should be(
      NonEmptyList[(String, ValueToken[Id])](
        ("a", toStr("str")),
        ("b", toNumber(3)) :: ("c", toNumber(5)) :: Nil
      )
    )
  }

  "one line struct value" should "be parsed" in {
    parseAndCheckStruct(
      """Obj(f1 = 1, f2 = "a", f3 = [1,2,3], f4=["b", "c"], f5 =NestedObj(i1 = 2, i2 = "b", i3= funcCall(3), i4 = value), f6=funcCall(1), f7 = Serv.call(2))"""
    )
  }

  "multiline line struct value" should "be parsed" in {
    parseAndCheckStruct(
      """Obj(f1 = 1,
        |f2 =
        |"a",
        |f3 = [1,2,3],
        |f4=["b",
        | "c"
        | ],
        | f5 =
        |    NestedObj(
        |       i1
        |         =
        |           2,
        |           i2 = "b", i3= funcCall(3), i4 = value), f6=funcCall(1), f7 = Serv.call(2))""".stripMargin
    )
  }

}
