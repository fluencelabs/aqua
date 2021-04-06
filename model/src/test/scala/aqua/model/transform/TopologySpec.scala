package aqua.model.transform

import aqua.model.Node
import aqua.model.body.FuncOp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TopologySpec extends AnyFlatSpec with Matchers {
  import Node._

  "topology resolver" should "do nothing on init peer" in {

    val init = on(
      initPeer,
      relay :: Nil,
      seq(
        call(1),
        call(2)
      )
    )

    val proc: Node = Topology.resolve(init)

    val expected = on(
      initPeer,
      relay :: Nil,
      seq(
        call(1, initPeer),
        call(2, initPeer)
      )
    )

    proc should be(expected)

  }

  "topology resolver" should "go through relay to any other node, directly" in {

    val init = on(
      initPeer,
      relay :: Nil,
      on(
        otherPeer,
        Nil,
        seq(
          call(1),
          call(2)
        )
      )
    )

    val proc: Node = Topology.resolve(init)

    val expected = on(
      initPeer,
      relay :: Nil,
      on(
        otherPeer,
        Nil,
        (FuncOp.noop(relay).tree: Node),
        seq(
          call(1, otherPeer),
          call(2, otherPeer)
        )
      )
    )

    proc should be(expected)
  }

  "topology resolver" should "go through relay to any other node, via another relay" in {

    val init = on(
      initPeer,
      relay :: Nil,
      on(
        otherPeer,
        otherRelay :: Nil,
        seq(
          call(1),
          call(2)
        )
      )
    )

    val proc: Node = Topology.resolve(init)

    val expected = on(
      initPeer,
      relay :: Nil,
      on(
        otherPeer,
        otherRelay :: Nil,
        (FuncOp.noop(relay).tree: Node),
        (FuncOp.noop(otherRelay).tree: Node),
        seq(
          call(1, otherPeer),
          call(2, otherPeer)
        )
      )
    )

//    println(Console.BLUE + init)
//    println(Console.YELLOW + proc)
//    println(Console.MAGENTA + expected)
//    println(Console.RESET)

    proc should be(expected)
  }

}
