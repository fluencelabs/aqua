package aqua.model.transform

import aqua.model.func.body.{CallFunctionTag, CallServiceTag, FuncOp}
import aqua.model.func.{ArgsDef, Call, FuncCallable}
import aqua.model.{LiteralModel, Node, VarModel}
import aqua.types.ScalarType
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
        through(relay),
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
        through(relay),
        through(otherRelay),
        seq(
          call(1, otherPeer),
          call(2, otherPeer)
        )
      )
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

    val expected = on(
      initPeer,
      relay :: Nil,
      seq(
        on(
          otherPeer,
          otherRelay :: Nil,
          through(relay),
          through(otherRelay),
          call(1, otherPeer)
        ),
        through(otherRelay),
        through(relay),
        call(2, initPeer)
      )
    )

//    println(Console.BLUE + init)
//    println(Console.YELLOW + proc)
//    println(Console.MAGENTA + expected)
//    println(Console.RESET)

    proc.equalsOrPrintDiff(expected) should be(true)
  }

}
