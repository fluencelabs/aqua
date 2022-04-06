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

class InfixTokenSpec extends AnyFlatSpec with Matchers with AquaSpec {

  def spanToId: Span.S ~> Id = new (Span.S ~> Id) {

    override def apply[A](span: Span.S[A]): Id[A] = {
      span._2
    }
  }

  private def literal(n: Int) = LiteralToken[Id](n.toString, LiteralType.number)

  import AquaSpec._

  "primitive math expression" should "be parsed" ignore {

    val vt = ValueToken.`_value`.parseAll("3 - 2").right.get.mapK(spanToId)

    val res = vtRotate(vt)
    res shouldBe
      InfixToken[Id](literal(3), literal(2), Sub)

  }

  "primitive math expression with multiplication" should "be parsed" ignore {

    val vt = ValueToken.`_value`.parseAll("(3 - 2) * 4").right.get.mapK(spanToId)
    val vt2 = ValueToken.`_value`.parseAll("3 - 2 * 4").right.get.mapK(spanToId)
    val vt3 = ValueToken.`_value`.parseAll("3 - 2 * 4 + 5 - 3 * 2").right.get.mapK(spanToId)
    val vt4 = ValueToken.`_value`.parseAll("3 * 2 - 4").right.get.mapK(spanToId)
    val vt5 = ValueToken.`_value`.parseAll("3 - 2 * 4 + 5").right.get.mapK(spanToId)

    val res = vtRotate(vt)

    val res2 = vtRotate(vt2)
    val res3 = vtRotate(vt3)
    val res4 = vtRotate(vt4)
    val res5 = vtRotate(vt5)

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

  "math expression" should "be parsed" ignore {

    val vt = ValueToken.`_value`.parseAll("3 - 2 + 5").right.get.mapK(spanToId)

    val res = vtRotate(vt)
    res shouldBe
      InfixToken(InfixToken(literal(3), literal(2), Sub), literal(5), Add)

  }

  "complex math expression" should "be parsed" ignore {

    val vt = ValueToken.`_value`.parseAll("(3 - 2 + 5) + 5 + (4 - 7)").right.get.mapK(spanToId)

    val res = vtRotate(vt)
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

  "complex math expression with multiplication" should "be parsed" ignore {

    val vt = ValueToken.`_value`.parseAll("(3 - 2) * 2 + (4 - 7) * 3").right.get.mapK(spanToId)

    val res = vtRotate(vt)
    res shouldBe
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

  /*
case (infix, right) => {
        case left: InfixToken[Span.S] if left.op.ordinal < infix._2.ordinal =>
          InfixToken(left.left, InfixToken(left.right, right, infix), left.infix)
        case left =>
          right match {
            case r: InfixToken[Span.S] if r.op.ordinal > infix._2.ordinal =>
              InfixToken(InfixToken(left, r.left, infix), r.right, r.infix)
            case _ =>
              InfixToken(left, right, infix)
          }
      }
   */

  // rotate tree of ops. Don't work properly with operations with weights
  // TODO: delete this
  def vtRotate[F[_]: Comonad](vt: ValueToken[F]): ValueToken[F] = {
    vt match {
      case BracketsToken(value) =>
        BracketsToken(vtRotate(value))
      case o @ InfixToken(l, r, op) =>
        println(o)
        val res = r match {
          case InfixToken(ll, rl, opl) if opl.extract.weight <= op.extract.weight =>
            val newT = InfixToken(vtRotate(l), vtRotate(ll), op)
            val newR = InfixToken(newT, rl, opl)
            vtRotate(newR)
          case b @ BracketsToken(value) =>
            o.copy(right = b.copy(value = vtRotate(value)))
          case vr =>
            InfixToken(vtRotate(l), vtRotate(vr), op)
        }

        res
      case v =>
        v
    }
  }

}
