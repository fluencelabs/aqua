package aqua.model.topology

import aqua.model.func.raw.SeqTag
import aqua.model.topology.ChainZipper.Matchers.`head`
import aqua.model.topology.Location.Matchers._
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LocationSpec extends AnyFlatSpec with Matchers {

  "matchers" should "unapply correctly" in {
    val loc =
      Location(ChainZipper.one(Cofree(SeqTag, Eval.later(Chain.empty[Topology.Tree]))) :: Nil)

    Option(loc).collect { case `head`(SeqTag) /: _ =>
      true
    } should be('defined)
  }

}
