package aqua.context

import aqua.context.walker.Walker
import aqua.parser.Block
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.Id
import cats.data.Validated.Invalid
import cats.data.{NonEmptyList, Validated}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless._

class ArrowsSpec extends AnyFlatSpec with Matchers with EitherValues {

  val walker = Walker.hnil[Id].andThen(new Arrows.ExpDef(_))

  def parseBlocks(str: String): List[Block[Id, Arrows[Id] :: HNil]] =
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

  "Arrows walker" should "collect no ability resolutions from a function" in {
    val bs = parseBlocks("""
                           |func some():
                           |   Peer "peer"
                           |   Peer.timestamp()
                           |
                           |""".stripMargin)

    bs.length should be(1)
    val ctx = bs.head.context
    val acc = ctx.head.expDef
    acc.expectAcc.keys should be('empty)
    acc.defineAcc.keys should be('empty)
  }

  "Arrows walker" should "collect ability expectations from two functions" in {
    val res = parseBlocksV("""
                             |func some():
                             |   x <- First.arr()
                             |
                             |
                             |func other(x: i32):
                             |   Peer "smth"
                             |   y <- Second.arr2(x)
                             |   Peer.timestamp()
                             |   
                             |""".stripMargin)

    res.isValid should be(false)
    val Invalid(errs) = res
    errs should have length (2)
  }

  "Arrows walker" should "resolve abilities in a function" in {
    val res = parseBlocksV("""
                             |func some():
                             |   y <- Smth.foo()
                             |   x <- Ab.f(z, y)
                             |   
                             |alias T: i32
                             |""".stripMargin)

    res.isValid should be(false)
    val Invalid(errs) = res
    errs should have length (2)
  }

}
