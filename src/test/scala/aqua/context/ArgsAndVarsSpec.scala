package aqua.context

import aqua.context.scope.{Scope, ScopeWalker}
import aqua.context.walker.Walker
import aqua.parser.Block
import cats.Id
import cats.data.{NonEmptyList, Validated}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.data.Validated.Invalid
import shapeless._

class ArgsAndVarsSpec extends AnyFlatSpec with Matchers with EitherValues {

  val walker =
    Walker.hnil[Id].andThen(new ScopeWalker(_)).andThen(new ArgsAndVars.ExpDef(_))

  def parseBlocks(str: String): List[Block[Id, ArgsAndVars[Id] :: Scope[Id] :: HNil]] =
    Validated
      .fromEither(Block.blocks[Id].parseAll(str))
      .leftMap(_.toString)
      .leftMap(NonEmptyList.one)
      .andThen(
        walker.walkValidate
      )
      .toEither
      .right
      .value

  def parseBlocksV(str: String) =
    Validated
      .fromEither(Block.blocks[Id].parseAll(str))
      .leftMap(_.toString)
      .leftMap(NonEmptyList.one)
      .andThen(
        walker.walkValidate
      )

  "Arguments and vars walker" should "collect no vars in a single function" in {
    val bs = parseBlocks("""
                           |func some():
                           |   x <- arr()
                           |
                           |""".stripMargin)

    bs.length should be(1)
    val ctx = bs.head.context
    ctx.tail.head should be(Scope[Id]())
    val acc = ctx.head.expDef
    acc.expectAcc.keys should be('empty)
    acc.defineAcc.keys should be('empty)
  }

  "Arguments and vars walker" should "collect no vars in two functions" in {
    val bs = parseBlocks("""
                           |func some():
                           |   x <- arr()
                           |
                           |
                           |func other(x: u32):
                           |   y <- arr2(x)
                           |   
                           |""".stripMargin)

    bs.length should be(2)
    val ctx = bs.last.context
    ctx.tail.head should be(Scope[Id]())
    val acc = ctx.head.expDef
    acc.expectAcc.keys should be('empty)
    acc.defineAcc.keys should be('empty)
  }

  "Arguments and vars walker" should "catch undefined var in a function" in {
    val res = parseBlocksV("""
                             |func some():
                             |   x <- f(z, y)
                             |   
                             |alias T: u32
                             |""".stripMargin)

    res.isValid should be(false)
    val Invalid(errs) = res
    errs should have length (2)
  }

}
