package aqua.model.topology

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Location.Matchers._
import ChainZipper.Matchers._
import aqua.model.func.body.SeqTag
import cats.Eval
import cats.data.Chain
import cats.free.Cofree

class LocationSpec extends AnyFlatSpec with Matchers {

  "matchers" should "unapply correctly" in {
    val loc =
      Location(ChainZipper.one(Cofree(SeqTag, Eval.later(Chain.empty[Topology.Tree]))) :: Nil)

    Option(loc).collect { case `head`(SeqTag) /: _ =>
      true
    } should be('defined)
  }

}
