package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.func.IfExpr
import aqua.parser.lexer.InfixToken.Op as InfixOp
import aqua.parser.lexer.PrefixToken.Op as PrefixOp
import aqua.parser.lexer.*
import aqua.parser.lexer.InfixToken.Op.*
import aqua.parser.lexer.PrefixToken.Op.*
import aqua.parser.lift.Span
import aqua.types.LiteralType

import cats.syntax.comonad.*
import cats.{~>, Comonad, Id}
import cats.parse.{Numbers, Parser as P, Parser0 as P0}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inside

class ValueTokenComplexSpec extends AnyFlatSpec with Matchers with Inside with AquaSpec {

  def spanToId: Span.S ~> Id = new (Span.S ~> Id) {

    override def apply[A](span: Span.S[A]): Id[A] = {
      span._2
    }
  }

  import AquaSpec.*

  private def variable(name: String): ValueToken[Id] =
    VarToken(Name(name), Nil)

  private def func(name: String, args: List[ValueToken[Id]]): ValueToken[Id] =
    CallArrowToken(None, Name(name), args)

  private def literal(n: Int): ValueToken[Id] = toNumber(n)

  private def literalBool(b: Boolean): ValueToken[Id] = toBool(b)

  private def literalString(s: String): ValueToken[Id] = toStr(s)

  "ValueToken" should "parse primitive math expression" in {

    val vt = ValueToken.`value`.parseAll("3").right.get.mapK(spanToId)
    val vt1 = ValueToken.`value`.parseAll("2 - 3").right.get.mapK(spanToId)
    val vt2 = ValueToken.`value`.parseAll("3 * 2 * 5").right.get.mapK(spanToId)

    val vt3 = ValueToken.`value`.parseAll("3 + 1 * 2 - 2").right.get.mapK(spanToId)
    val vt4 = ValueToken.`value`.parseAll("5 + 6 + 10 + 20 + 1 + 2 + 4").right.get.mapK(spanToId)
    val vt5 = ValueToken.`value`.parseAll("3 * 2 * 5 + 3").right.get.mapK(spanToId)
    val vt6 = ValueToken.`value`.parseAll("2 + 3 * 2 - 3 * 5 + 2").right.get.mapK(spanToId)
    val vt7 = ValueToken.`value`.parseAll("2 * 3 / 5 * 4").right.get.mapK(spanToId)
    val vt8 = ValueToken.`value`
      .parseAll("3 - 2 + 3 * 3 / 5 * 2 - 4 / 4 ** 2 ** 2 % 2 + 1 - 4 > 2 * 3 - 5 + 1 * 2 / 2")
      .right
      .get
      .mapK(spanToId)
    val vt9 = ValueToken.`value`
      .parseAll("5 > 4")
      .right
      .get
      .mapK(spanToId)
    val vt10 = ValueToken.`value`
      .parseAll("2 ** 3 ** 4")
      .right
      .get
      .mapK(spanToId)
    val vt11 = ValueToken.`value`
      .parseAll("2 % 4")
      .right
      .get
      .mapK(spanToId)

    vt shouldBe literal(3)
    vt1 shouldBe sub(literal(2), literal(3))
    vt2 shouldBe mul(mul(3, 2), 5)
    vt3 shouldBe sub(add(3, mul(1, 2)), 2)
    vt4 shouldBe add(add(add(add(add(add(5, 6), 10), 20), 1), 2), 4)
    vt5 shouldBe add(mul(mul(3, 2), 5), 3)
    vt6 shouldBe add(sub(add(2, mul(3, 2)), mul(3, 5)), 2)
    vt7 shouldBe mul(div(mul(2, 3), 5), 4)
    vt9 shouldBe gt(5, 4)
    vt10 shouldBe pow(2, pow(3, 4))
    vt11 shouldBe rem(2, 4)
  }

  it should "parse primitive math expression with multiplication" in {

    val res = ValueToken.`value`.parseAll("(3 - 2) * 4").right.get.mapK(spanToId)
    val res2 = ValueToken.`value`.parseAll("3 - 2 * 4").right.get.mapK(spanToId)
    val res3 = ValueToken.`value`.parseAll("3 - 2 * 4 + 5 - 3 * 2").right.get.mapK(spanToId)
    val res4 = ValueToken.`value`.parseAll("3 * 2 - 4").right.get.mapK(spanToId)
    val res5 = ValueToken.`value`.parseAll("3 - 2 * 4 + 5").right.get.mapK(spanToId)

    res shouldBe
      InfixToken(
        InfixToken(literal(3), literal(2), Sub),
        literal(4),
        Mul
      )

    res2 shouldBe
      InfixToken(
        literal(3),
        InfixToken(literal(2), literal(4), Mul),
        Sub
      )

    res4 shouldBe
      InfixToken(
        InfixToken(literal(3), literal(2), Mul),
        literal(4),
        Sub
      )

    res5 shouldBe
      InfixToken(
        InfixToken(
          literal(3),
          InfixToken(literal(2), literal(4), Mul),
          Sub
        ),
        literal(5),
        Add
      )

    res3 shouldBe
      InfixToken(
        InfixToken(
          InfixToken(
            literal(3),
            InfixToken(literal(2), literal(4), Mul),
            Sub
          ),
          literal(5),
          Add
        ),
        InfixToken(literal(3), literal(2), Mul),
        Sub
      )

  }

  it should "parse math expression" in {

    val vt = ValueToken.`value`.parseAll("3 - 2 + 5").right.get.mapK(spanToId)

    vt shouldBe
      InfixToken(InfixToken(literal(3), literal(2), Sub), literal(5), Add)

  }

  it should "parse complex math expression" in {

    val res = ValueToken.`value`.parseAll("(3 - 2 + 5) + 5 + (4 - 7)").right.get.mapK(spanToId)

    res shouldBe
      InfixToken(
        InfixToken(
          InfixToken(InfixToken(literal(3), literal(2), Sub), literal(5), Add),
          literal(5),
          Add
        ),
        InfixToken(literal(4), literal(7), Sub),
        Add
      )
  }

  it should "parse complex math expression with multiplication" in {

    val vt = ValueToken.`value`.parseAll("(3 - 2) * 2 + (4 - 7) * 3").right.get.mapK(spanToId)

    vt shouldBe
      InfixToken(
        InfixToken(
          InfixToken(literal(3), literal(2), Sub),
          literal(2),
          Mul
        ),
        InfixToken(
          InfixToken(literal(4), literal(7), Sub),
          literal(3),
          Mul
        ),
        Add
      )
  }

  it should "parse simple math expression with exp" in {
    // Correct (1 ** (2 ** 3))
    val vt = ValueToken.`value`.parseAll("1**2**3").right.get.mapK(spanToId)

    vt shouldBe
      InfixToken(literal(1), InfixToken(literal(2), literal(3), Pow), Pow)

  }

  it should "parse complex math expression with exp" in {
    // Correct ((1 ** 2) + (((3 ** 4) * (5 ** (6 ** 7))) * 9))
    val vt = ValueToken.`value`.parseAll("1 ** 2 + 3**4* 5**6 ** 7*9").right.get.mapK(spanToId)

    vt shouldBe
      InfixToken(
        InfixToken(
          literal(1),
          literal(2),
          Pow
        ),
        InfixToken(
          InfixToken(
            InfixToken(
              literal(3),
              literal(4),
              Pow
            ),
            InfixToken(literal(5), InfixToken(literal(6), literal(7), Pow), Pow),
            Mul
          ),
          literal(9),
          Mul
        ),
        Add
      )

  }

  it should "parse simple cmp math expression" in {
    val vt = ValueToken.`value`.parseAll("1 > 3").right.get.mapK(spanToId)
    val vt1 = ValueToken.`value`.parseAll("1 < 3").right.get.mapK(spanToId)
    val vt2 = ValueToken.`value`.parseAll("1 >= 3").right.get.mapK(spanToId)
    val vt3 = ValueToken.`value`.parseAll("1 <= 3").right.get.mapK(spanToId)

    vt shouldBe gt(literal(1), literal(3))
    vt1 shouldBe lt(literal(1), literal(3))
    vt2 shouldBe gte(literal(1), literal(3))
    vt3 shouldBe lte(literal(1), literal(3))
  }

  it should "parse complex cmp math expression" in {
    val test = (op: InfixOp) => {
      val vt = ValueToken.`value`
        .parseAll(
          s"(1 + 2) ** 3 ${op.symbol} 4 - 5 * 6"
        )
        .right
        .get
        .mapK(spanToId)
      val left = pow(add(literal(1), literal(2)), literal(3))
      val right = sub(literal(4), mul(literal(5), literal(6)))
      val exp = infixToken(left, right, op)
      vt shouldBe exp
    }

    List(Gt, Lt, Gte, Lte).foreach(test)
  }

  it should "parse simple cmp math expression in brackets" in {
    val vt = ValueToken.`value`.parseAll("(1 > 3)").right.get.mapK(spanToId)
    vt shouldBe InfixToken(literal(1), literal(3), Gt)
  }

  it should "parse simple logical expression" in {
    val vtAnd = ValueToken.`value`.parseAll("true && false").map(_.mapK(spanToId))

    inside(vtAnd) { case Right(vt) =>
      vt shouldBe infixToken(literalBool(true), literalBool(false), And)
    }

    val vtOr = ValueToken.`value`.parseAll("false || true").map(_.mapK(spanToId))

    inside(vtOr) { case Right(vt) =>
      vt shouldBe infixToken(literalBool(false), literalBool(true), Or)
    }

    val vtAndOr = ValueToken.`value`.parseAll("false && true || false").map(_.mapK(spanToId))

    inside(vtAndOr) { case Right(vt) =>
      vt shouldBe infixToken(
        infixToken(literalBool(false), literalBool(true), And),
        literalBool(false),
        Or
      )
    }

    val vtOrAnd = ValueToken.`value`.parseAll("false || true && false").map(_.mapK(spanToId))

    inside(vtOrAnd) { case Right(vt) =>
      vt shouldBe infixToken(
        literalBool(false),
        infixToken(literalBool(true), literalBool(false), And),
        Or
      )
    }

    val vtOrNotAnd = ValueToken.`value`.parseAll("false || !true && false").map(_.mapK(spanToId))

    inside(vtOrNotAnd) { case Right(vt) =>
      vt shouldBe infixToken(
        literalBool(false),
        infixToken(
          prefixToken(literalBool(true), Not),
          literalBool(false),
          And
        ),
        Or
      )
    }
  }

  it should "parse simple equality expression" in {
    val ltEqLt = ValueToken.`value`.parseAll("\"abc\" == \"cba\"").map(_.mapK(spanToId))

    inside(ltEqLt) { case Right(vt) =>
      vt shouldBe equ(literalString("abc"), literalString("cba"))
    }

    val vtNeqLt = ValueToken.`value`.parseAll("a != \"cba\"").map(_.mapK(spanToId))

    inside(vtNeqLt) { case Right(vt) =>
      vt shouldBe neq(variable("a"), literalString("cba"))
    }
  }

  it should "parse logical expression with brackets" in {
    val vtAndOr = ValueToken.`value`.parseAll("false && (true || false)").map(_.mapK(spanToId))

    inside(vtAndOr) { case Right(vt) =>
      vt shouldBe infixToken(
        literalBool(false),
        infixToken(literalBool(true), literalBool(false), Or),
        And
      )
    }

    val vtOrAnd = ValueToken.`value`.parseAll("(false || true) && false").map(_.mapK(spanToId))

    inside(vtOrAnd) { case Right(vt) =>
      vt shouldBe infixToken(
        infixToken(literalBool(false), literalBool(true), Or),
        literalBool(false),
        And
      )
    }

    val vtNotAndOr = ValueToken.`value`.parseAll("!false && (true || false)").map(_.mapK(spanToId))

    inside(vtNotAndOr) { case Right(vt) =>
      vt shouldBe infixToken(
        prefixToken(literalBool(false), Not),
        infixToken(literalBool(true), literalBool(false), Or),
        And
      )
    }

    val vtNotOrAnd = ValueToken.`value`.parseAll("!(false || true) && false").map(_.mapK(spanToId))

    inside(vtNotOrAnd) { case Right(vt) =>
      vt shouldBe infixToken(
        prefixToken(
          infixToken(literalBool(false), literalBool(true), Or),
          Not
        ),
        literalBool(false),
        And
      )
    }
  }

  it should "parse logical expression with math expressions" in {
    val vt1 = ValueToken.`value`.parseAll("1 < 2 + 3 || 3 % 2 > 1").map(_.mapK(spanToId))

    inside(vt1) { case Right(vt) =>
      vt shouldBe or(
        lt(
          literal(1),
          add(literal(2), literal(3))
        ),
        gt(
          rem(literal(3), literal(2)),
          literal(1)
        )
      )
    }

    val vt2 = ValueToken.`value`.parseAll("1 - 2 > 3 && 3 ** 2 <= 1").map(_.mapK(spanToId))

    inside(vt2) { case Right(vt) =>
      vt shouldBe and(
        gt(
          sub(literal(1), literal(2)),
          literal(3)
        ),
        lte(
          pow(literal(3), literal(2)),
          literal(1)
        )
      )
    }

    val vt3 = ValueToken.`value`.parseAll("!(1 - 2 > 3) && 3 ** 2 <= 1").map(_.mapK(spanToId))

    inside(vt3) { case Right(vt) =>
      vt shouldBe and(
        not(
          gt(
            sub(literal(1), literal(2)),
            literal(3)
          )
        ),
        lte(
          pow(literal(3), literal(2)),
          literal(1)
        )
      )
    }
  }

  it should "parse logical expression with math and equality" in {
    val vt1 = ValueToken.`value`.parseAll("1 == 2 + 3 || 3 % 2 != 1").map(_.mapK(spanToId))

    inside(vt1) { case Right(vt) =>
      vt shouldBe or(
        equ(
          literal(1),
          add(literal(2), literal(3))
        ),
        neq(
          rem(literal(3), literal(2)),
          literal(1)
        )
      )
    }

    val vt2 = ValueToken.`value`.parseAll("1 - 2 != 3 && 3 ** 2 == 1").map(_.mapK(spanToId))

    inside(vt2) { case Right(vt) =>
      vt shouldBe and(
        neq(
          sub(literal(1), literal(2)),
          literal(3)
        ),
        equ(
          pow(literal(3), literal(2)),
          literal(1)
        )
      )
    }

    val vt3 =
      ValueToken.`value`.parseAll("!(true == 2 > 3) && false == 2 <= 1").map(_.mapK(spanToId))

    inside(vt3) { case Right(vt) =>
      vt shouldBe and(
        not(
          equ(
            literalBool(true),
            gt(literal(2), literal(3))
          )
        ),
        equ(
          literalBool(false),
          lte(literal(2), literal(1))
        )
      )
    }
  }

  it should "parse logical expression with function calls and variables" in {
    val vt1 = ValueToken.`value`.parseAll("foo() || a + 1 < 2 && b").map(_.mapK(spanToId))

    inside(vt1) { case Right(vt) =>
      vt shouldBe or(
        func("foo", Nil),
        and(
          lt(
            add(
              variable("a"),
              literal(1)
            ),
            literal(2)
          ),
          variable("b")
        )
      )
    }

    val vt2 = ValueToken.`value`.parseAll("bar(a) < 2 && (b > 5 || c)").map(_.mapK(spanToId))

    inside(vt2) { case Right(vt) =>
      vt shouldBe and(
        lt(func("bar", List(variable("a"))), literal(2)),
        or(
          gt(
            variable("b"),
            literal(5)
          ),
          variable("c")
        )
      )
    }

    val vt3 = ValueToken.`value`.parseAll("!baz(a) && (!(b > 4) || !c)").map(_.mapK(spanToId))

    inside(vt3) { case Right(vt) =>
      vt shouldBe and(
        not(func("baz", List(variable("a")))),
        or(
          not(
            gt(
              variable("b"),
              literal(4)
            )
          ),
          prefixToken(variable("c"), Not)
        )
      )
    }

    val vt4 = ValueToken.`value`.parseAll("a == foo(b) && !(baz(c) != d)").map(_.mapK(spanToId))

    inside(vt4) { case Right(vt) =>
      vt shouldBe and(
        equ(
          variable("a"),
          func("foo", List(variable("b")))
        ),
        not(
          neq(
            func("baz", List(variable("c"))),
            variable("d")
          )
        )
      )
    }

    val vt5 =
      ValueToken.`value`.parseAll("!(a == foo(b)) || baz(c) == (d && e)").map(_.mapK(spanToId))

    inside(vt5) { case Right(vt) =>
      vt shouldBe or(
        not(
          equ(
            variable("a"),
            func("foo", List(variable("b")))
          )
        ),
        equ(
          func("baz", List(variable("c"))),
          and(
            variable("d"),
            variable("e")
          )
        )
      )
    }
  }

}
