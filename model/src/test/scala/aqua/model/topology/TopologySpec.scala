package aqua.model.topology

import aqua.model.Node
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

    val expected =
      seq(
        call(1, initPeer),
        call(2, initPeer)
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

    val expected =
      seq(through(relay), call(1, otherPeer), call(2, otherPeer))

    proc should be(expected)
  }

  "topology resolver" should "simplify a route" in {
    val init = on(
      initPeer,
      relay :: Nil,
      seq(
        on(
          initPeer,
          relay :: Nil,
          call(1)
        ),
        call(3)
      )
    )

    val proc: Node = Topology.resolve(init)

    println(proc)
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

    val expected =
      seq(
        through(relay),
        through(otherRelay),
        call(1, otherPeer),
        call(2, otherPeer)
      )

    proc should be(expected)
  }

  "topology resolver" should "get back to init peer" in {

    val init = on(
      initPeer,
      relay :: Nil,
      seq(
        on(
          otherPeer,
          otherRelay :: Nil,
          call(1)
        ),
        call(2)
      )
    )

    val proc: Node = Topology.resolve(init)

    val expected =
      seq(
        through(relay),
        through(otherRelay),
        call(1, otherPeer),
        through(otherRelay),
        through(relay),
        call(2, initPeer)
      )

//    println(Console.BLUE + init)
//    println(Console.YELLOW + proc)
//    println(Console.MAGENTA + expected)
//    println(Console.RESET)

    proc.equalsOrPrintDiff(expected) should be(true)
  }

  "topology resolver" should "get back to init peer after a long chain" in {

    val init = on(
      initPeer,
      relay :: Nil,
      seq(
        on(
          otherPeer,
          otherRelay :: Nil,
          call(0),
          on(
            otherPeer2,
            otherRelay :: Nil,
            call(1),
            _match(
              otherPeer,
              otherRelay,
              on(
                otherPeer,
                otherRelay :: Nil,
                call(2)
              )
            )
          )
        ),
        call(3)
      )
    )

    val proc: Node = Topology.resolve(init)

    val expected =
      seq(
        through(relay),
        through(otherRelay),
        call(0, otherPeer),
        through(otherRelay),
        call(1, otherPeer2),
        _match(
          otherPeer,
          otherRelay,
          seq(
            through(otherRelay),
            call(2, otherPeer)
          )
        ),
        through(otherRelay),
        through(relay),
        call(3, initPeer)
      )

//    println(Console.BLUE + init)
//    println(Console.YELLOW + proc)
//    println(Console.MAGENTA + expected)
//    println(Console.RESET)

    proc.equalsOrPrintDiff(expected) should be(true)
  }

}
