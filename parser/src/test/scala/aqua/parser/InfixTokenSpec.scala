package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.func.IfExpr
import aqua.parser.lexer.{BracketsToken, EqOp, InfixToken, LiteralToken, ValueToken}
import aqua.parser.lexer.InfixToken.Op.*
import aqua.parser.lift.Span
import aqua.types.LiteralType
import cats.{~>, Comonad, Id}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InfixTokenSpec extends AnyFlatSpec with Matchers with AquaSpec {

  def spanToId: Span.S ~> Id = new (Span.S ~> Id) {

    override def apply[A](span: Span.S[A]): Id[A] = {
      span._2
    }
  }

  def literal(n: Int) = LiteralToken[Id](n.toString, LiteralType.number)

  import AquaSpec._

  ignore should "be parsed" in {

    val a = ValueToken.`_value`.parseAll("(3 - 2 + 5) + 5 + (4 - 7)")

    val res = vtRotate(a.right.get)
    res shouldBe (
      InfixToken(
        InfixToken(
          BracketsToken(InfixToken(InfixToken(literal(3), literal(2), Sub), literal(5), Add)),
          literal(5),
          Add
        ),
        BracketsToken(InfixToken(literal(4), literal(7), Sub)),
        Add
      )
    )
  }

  // rotate tree of ops. Don't work properly with operations with weights
  def vtRotate[F[_]: Comonad](vt: ValueToken[F]): ValueToken[F] = {
    vt match {
      case BracketsToken(value) =>
        BracketsToken(vtRotate(value))
      case o @ InfixToken(l, r, op) =>
        val res = r match {
          case InfixToken(ll, rl, opl) =>
            val newRR = vtRotate(rl)
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
