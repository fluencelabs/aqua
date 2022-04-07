package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.func.IfExpr
import aqua.parser.lexer.{BracketsToken, EqOp, InfixToken, LiteralToken, ValueToken}
import aqua.parser.lexer.InfixToken.Op.*
import aqua.parser.lift.Span
import aqua.types.LiteralType
import cats.syntax.comonad.*
import cats.{~>, Comonad, Id}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.parse.{Numbers, Parser as P, Parser0 as P0}

class InfixTokenSpec extends AnyFlatSpec with Matchers with AquaSpec {

  def spanToId: Span.S ~> Id = new (Span.S ~> Id) {

    override def apply[A](span: Span.S[A]): Id[A] = {
      span._2
    }
  }

  private def literal(n: Int) = LiteralToken[Id](n.toString, LiteralType.number)

  import AquaSpec._

  "primitive math expression" should "be parfvfsed" in {

    val vt = ValueToken.atom.parseAll("3").right.get.mapK(spanToId)
    val vt2 = InfixToken.mult.parseAll("3 * 2 * 5").right.get.mapK(spanToId)

    val vt31 = ValueToken.`_value`.parseAll("3 + 1 * 2 - 2").right.get.mapK(spanToId)
    val vt32 = ValueToken.`_value`.parseAll("5 + 6 + 10 + 20 + 1 + 2 + 4").right.get.mapK(spanToId)
    val vt4 = ValueToken.`_value`.parseAll("3 * 2 * 5 + 3").right.get.mapK(spanToId)
    val vt5 = ValueToken.`_value`.parseAll("2 + 3 * 2 - 3 * 5 + 2").right.get.mapK(spanToId)
    val vt6 = ValueToken.`_value`.parseAll("2 * 3 / 5 * 4").right.get.mapK(spanToId)
    val vt7 = ValueToken.`_value`
      .parseAll("3 - 2 + 3 * 3 / 5 * 2 - 4 / 4 ** 2 ** 2 % 2 + 1 - 4 > 2 * 3 - 5 + 1 * 2 / 2")
    val vt8 = ValueToken.`_value`
      .parseAll("5 > 4")

    println(vt7)
    println(vt8)
  }

  "primitive math expression" should "be parsed" in {

    val vt = ValueToken.`_value`.parseAll("3 - 2").right.get.mapK(spanToId)

    vt shouldBe
      InfixToken[Id](literal(3), literal(2), Sub)

  }

  "primitive math expression with multiplication" should "be parsed" in {

    val res = ValueToken.`_value`.parseAll("(3 - 2) * 4").right.get.mapK(spanToId)
    val res2 = ValueToken.`_value`.parseAll("3 - 2 * 4").right.get.mapK(spanToId)
    val res3 = ValueToken.`_value`.parseAll("3 - 2 * 4 + 5 - 3 * 2").right.get.mapK(spanToId)
    val res4 = ValueToken.`_value`.parseAll("3 * 2 - 4").right.get.mapK(spanToId)
    val res5 = ValueToken.`_value`.parseAll("3 - 2 * 4 + 5").right.get.mapK(spanToId)

    res shouldBe
      InfixToken(
        BracketsToken(InfixToken(literal(3), literal(2), Sub)),
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

    val vt = ValueToken.`_value`.parseAll("3 - 2 + 5").right.get.mapK(spanToId)

    vt shouldBe
      InfixToken(InfixToken(literal(3), literal(2), Sub), literal(5), Add)

  }

  "complex math expression" should "be parsed" in {

    val res = ValueToken.`_value`.parseAll("(3 - 2 + 5) + 5 + (4 - 7)").right.get.mapK(spanToId)

    res shouldBe
      InfixToken(
        InfixToken(
          BracketsToken(InfixToken(InfixToken(literal(3), literal(2), Sub), literal(5), Add)),
          literal(5),
          Add
        ),
        BracketsToken(InfixToken(literal(4), literal(7), Sub)),
        Add
      )
  }

  "complex math expression with multiplication" should "be parsed" in {

    val vt = ValueToken.`_value`.parseAll("(3 - 2) * 2 + (4 - 7) * 3").right.get.mapK(spanToId)

    vt shouldBe
      InfixToken(
        InfixToken(
          BracketsToken(InfixToken(literal(3), literal(2), Sub)),
          literal(2),
          Mul
        ),
        InfixToken(
          BracketsToken(InfixToken(literal(4), literal(7), Sub)),
          literal(3),
          Mul
        ),
        Add
      )
  }

}
