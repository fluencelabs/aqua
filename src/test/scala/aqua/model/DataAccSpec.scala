package aqua.model

import aqua.parser.Block
import cats.Id
import cats.data.{NonEmptyList, Validated}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import shapeless._

class DataAccSpec extends AnyFlatSpec with Matchers with EitherValues {

  val passer = Passer.hnil[Id].andThen(new ScopePasser(_)).andThen(new DataAcc.Pass(_))

  def parseBlocks(str: String): List[Block[Id, DataAcc[Id] :: Scope[Id] :: HNil]] =
    Validated
      .fromEither(Block.blocks[Id].parseAll(str))
      .leftMap(_.toString)
      .leftMap(NonEmptyList.one)
      .andThen(
        passer.pass
      )
      .toEither
      .right
      .value

  "data acc" should "collect vars" in {
    val bs = parseBlocks("""
                           |func some():
                           |   x <- arr()
                           |
                           |""".stripMargin)

    bs.length should be(1)
    val ctx = bs.head.context
    ctx.tail.head should be(Scope[Id]())
    val acc = ctx.head.acc
    acc.in.keys should be('empty)
    acc.out.keys should be('empty)
  }

}
