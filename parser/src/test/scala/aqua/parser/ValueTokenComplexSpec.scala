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

  import AquaSpec._

  private def variable(name: String): ValueToken[Id] =
    VarToken(Name(name), Nil)

  private def func(name: String, args: List[ValueToken[Id]]): ValueToken[Id] =
    CallArrowToken(None, Name(name), args)

  private def literal(n: Int): ValueToken[Id] = toNumber(n)

  private def literalBool(b: Boolean): ValueToken[Id] = toBool(b)

  private def prefixToken(value: ValueToken[Id], op: PrefixOp) =
    PrefixToken[Id](value, op)

  private def infixToken(left: ValueToken[Id], right: ValueToken[Id], op: InfixOp) =
    InfixToken[Id](left, right, op)

  private def mul(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Mul)

  private def sub(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Sub)

  private def div(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Div)

  private def rem(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Rem)

  private def add(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Add)

  private def pow(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Pow)

  private def gt(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Gt)

  private def gte(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Gte)

  private def lt(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Lt)

  private def lte(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Lte)

  "primitive math expression" should "be parsed" in {

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

  "primitive math expression with multiplication" should "be parsed" in {

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

  "math expression" should "be parsed" in {

    val vt = ValueToken.`value`.parseAll("3 - 2 + 5").right.get.mapK(spanToId)

    vt shouldBe
      InfixToken(InfixToken(literal(3), literal(2), Sub), literal(5), Add)

  }

  "complex math expression" should "be parsed" in {

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

  "complex math expression with multiplication" should "be parsed" in {

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

  "simple math expression with exp" should "be parsed" in {
    // Correct (1 ** (2 ** 3))
    val vt = ValueToken.`value`.parseAll("1**2**3").right.get.mapK(spanToId)

    vt shouldBe
      InfixToken(literal(1), InfixToken(literal(2), literal(3), Pow), Pow)

  }

  "complex math expression with exp" should "be parsed" in {
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

  "simple cmp math expression " should "be parsed" in {
    val vt = ValueToken.`value`.parseAll("1 > 3").right.get.mapK(spanToId)
    val vt1 = ValueToken.`value`.parseAll("1 < 3").right.get.mapK(spanToId)
    val vt2 = ValueToken.`value`.parseAll("1 >= 3").right.get.mapK(spanToId)
    val vt3 = ValueToken.`value`.parseAll("1 <= 3").right.get.mapK(spanToId)

    vt shouldBe gt(literal(1), literal(3))
    vt1 shouldBe lt(literal(1), literal(3))
    vt2 shouldBe gte(literal(1), literal(3))
    vt3 shouldBe lte(literal(1), literal(3))
  }

  "complex cmp math expression " should "be parsed" in {
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

  "simple cmp math expression in brackets " should "be parsed" in {
    val vt = ValueToken.`value`.parseAll("(1 > 3)").right.get.mapK(spanToId)
    vt shouldBe InfixToken(literal(1), literal(3), Gt)
  }

  "simple logical expression" should "be parsed" in {
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

  "logical expression with brackets" should "be parsed" in {
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

  "logical expression with math expressions" should "be parsed" in {
    val vt1 = ValueToken.`value`.parseAll("1 < 2 + 3 || 3 % 2 > 1").map(_.mapK(spanToId))

    inside(vt1) { case Right(vt) =>
      vt shouldBe infixToken(
        infixToken(
          literal(1),
          infixToken(literal(2), literal(3), Add),
          Lt
        ),
        infixToken(
          infixToken(literal(3), literal(2), Rem),
          literal(1),
          Gt
        ),
        Or
      )
    }

    val vt2 = ValueToken.`value`.parseAll("1 - 2 > 3 && 3 ** 2 <= 1").map(_.mapK(spanToId))

    inside(vt2) { case Right(vt) =>
      vt shouldBe infixToken(
        infixToken(
          infixToken(literal(1), literal(2), Sub),
          literal(3),
          Gt
        ),
        infixToken(
          infixToken(literal(3), literal(2), Pow),
          literal(1),
          Lte
        ),
        And
      )
    }

    val vt3 = ValueToken.`value`.parseAll("!(1 - 2 > 3) && 3 ** 2 <= 1").map(_.mapK(spanToId))

    inside(vt3) { case Right(vt) =>
      vt shouldBe infixToken(
        prefixToken(
          infixToken(
            infixToken(literal(1), literal(2), Sub),
            literal(3),
            Gt
          ),
          Not
        ),
        infixToken(
          infixToken(literal(3), literal(2), Pow),
          literal(1),
          Lte
        ),
        And
      )
    }
  }

  "logical expression with function calls and variables" should "be parsed" in {
    val vt1 = ValueToken.`value`.parseAll("foo() || a + 1 < 2 && b").map(_.mapK(spanToId))

    inside(vt1) { case Right(vt) =>
      vt shouldBe infixToken(
        func("foo", Nil),
        infixToken(
          infixToken(
            infixToken(
              variable("a"),
              literal(1),
              Add
            ),
            literal(2),
            Lt
          ),
          variable("b"),
          And
        ),
        Or
      )
    }

    val vt2 = ValueToken.`value`.parseAll("bar(a) < 2 && (b > 5 || c)").map(_.mapK(spanToId))

    inside(vt2) { case Right(vt) =>
      vt shouldBe infixToken(
        infixToken(func("bar", List(variable("a"))), literal(2), Lt),
        infixToken(
          infixToken(
            variable("b"),
            literal(5),
            Gt
          ),
          variable("c"),
          Or
        ),
        And
      )
    }

    val vt3 = ValueToken.`value`.parseAll("!baz(a) && (!(b > 4) || !c)").map(_.mapK(spanToId))

    inside(vt3) { case Right(vt) =>
      vt shouldBe infixToken(
        prefixToken(func("baz", List(variable("a"))), Not),
        infixToken(
          prefixToken(
            infixToken(
              variable("b"),
              literal(4),
              Gt
            ),
            Not
          ),
          prefixToken(variable("c"), Not),
          Or
        ),
        And
      )
    }
  }

}