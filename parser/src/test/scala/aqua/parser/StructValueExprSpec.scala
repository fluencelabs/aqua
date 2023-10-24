package aqua.parser

import aqua.AquaSpec
import aqua.AquaSpec.{toNumber, toStr, toVar}
import aqua.parser.expr.ConstantExpr
import aqua.parser.expr.func.AssignmentExpr
import aqua.parser.lexer.*
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
    val a = toStr("a")
    val b = toStr("b")
    val c = toStr("c")

    parseData(
      str
    ) should be(
      NamedValueToken(
        NamedTypeToken[Id]("Obj"),
        NonEmptyList.of(
          NamedArg.Full(toName("f1"), one),
          NamedArg.Full(toName("f2"), a),
          NamedArg.Full(toName("f3"), CollectionToken[Id](ArrayMode, List(one, two, three))),
          NamedArg.Full(toName("f4"), CollectionToken[Id](ArrayMode, List(b, c))),
          NamedArg.Full(
            toName("f5"),
            NamedValueToken(
              NamedTypeToken[Id]("NestedObj"),
              NonEmptyList.of(
                NamedArg.Full(toName("i1"), two),
                NamedArg.Full(toName("i2"), b),
                NamedArg.Full(toName("i3"), CallArrowToken(toName("funcCall"), List(three))),
                NamedArg.Full(toName("i4"), VarToken(toName("value")))
              )
            )
          ),
          NamedArg.Full(toName("f6"), CallArrowToken(Name[Id]("funcCall"), List(one))),
          NamedArg.Full(
            toName("f7"),
            PropertyToken[Id](
              VarToken[Id](Name[Id]("Serv")),
              NonEmptyList.one(IntoArrow[Id](Name[Id]("call"), List(two)))
            )
          )
        )
      )
    )
  }

  "named args" should "parse one full named arg" in {
    val result = NamedArg.namedArg
      .parseAll("""  a
                  | =
                  |  3""".stripMargin)
      .map(_.mapK(spanToId))
      .value

    result should be(NamedArg.Full(toName("a"), toNumber(3)))
  }

  it should "parse one short named arg" in {
    val result = NamedArg.namedArg
      .parseAll("  b ")
      .map(_.mapK(spanToId))
      .value

    result should be(NamedArg.Short(toVar("b")))
  }

  it should "parse a few full named args" in {
    val result = NamedArg.namedArgs
      .parseAll("""(
                  |a = "str",
                  |b = 3,
                  |c
                  |  =
                  |    5
                  |)""".stripMargin)
      .value
      .map(_.mapK(spanToId))

    result should be(
      NonEmptyList.of(
        NamedArg.Full(toName("a"), toStr("str")),
        NamedArg.Full(toName("b"), toNumber(3)),
        NamedArg.Full(toName("c"), toNumber(5))
      )
    )
  }

  it should "parse a few short named args" in {
    val result = NamedArg.namedArgs
      .parseAll("""(
                  |a,
                  | b ,
                  |c
                  |
                  |)""".stripMargin)
      .value
      .map(_.mapK(spanToId))

    result should be(
      NonEmptyList.of(
        NamedArg.Short(toVar("a")),
        NamedArg.Short(toVar("b")),
        NamedArg.Short(toVar("c"))
      )
    )
  }

  it should "parse mixed named args" in {
    val args = List(
      "meaning = 42" -> NamedArg.Full(toName("meaning"), toNumber(42)),
      "variable" -> NamedArg.Short(toVar("variable")),
      "col = [1,2,3]" -> NamedArg.Full(
        toName("col"),
        CollectionToken[Id](ArrayMode, List(toNumber(1), toNumber(2), toNumber(3)))
      ),
      "arrow" -> NamedArg.Short(toVar("arrow"))
    )

    args.permutations.foreach(perm =>
      val str = perm.map(_._1).mkString("(", ", ", ")")
      val expected = NonEmptyList.fromListUnsafe(perm.map(_._2))

      val result = NamedArg.namedArgs
        .parseAll(str)
        .value
        .map(_.mapK(spanToId))

      result should be(expected)
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
