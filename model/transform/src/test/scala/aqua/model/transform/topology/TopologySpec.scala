package aqua.model.transform.topology

import aqua.model.transform.ModelBuilder
import aqua.model.*
import aqua.res.*
import aqua.raw.ops.Call
import aqua.raw.value.{IntoIndexRaw, LiteralRaw, VarRaw}
import aqua.types.{LiteralType, ScalarType, StreamType}
import cats.Eval
import cats.data.{Chain, NonEmptyList}
import cats.free.Cofree
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.syntax.show.*

class TopologySpec extends AnyFlatSpec with Matchers {

  import ModelBuilder._

  "topology resolver" should "do nothing on init peer" in {

    val init = OnModel(
      rawToValue(initPeer),
      Chain.one(rawToValue(relay))
    ).wrap(
      SeqModel.wrap(
        callModel(1),
        callModel(2)
      )
    )

    val proc = Topology.resolve(init).value

    val expected =
      SeqRes.wrap(
        callRes(1, initPeer),
        callRes(2, initPeer)
      )

    proc.equalsOrShowDiff(expected) should be(true)
  }

  "topology resolver" should "go through relay to any other node, directly" in {

    val init = OnModel(initPeer, Chain.one(relay)).wrap(
      OnModel(otherPeer, Chain.empty).wrap(
        SeqModel.wrap(
          callModel(1),
          callModel(2)
        )
      )
    )

    val proc = Topology.resolve(init).value

    val expected =
      SeqRes.wrap(through(relay), callRes(1, otherPeer), callRes(2, otherPeer))

    proc.equalsOrShowDiff(expected) should be(true)
  }

  "topology resolver" should "go through relay to any other node, via another relay" in {

    val init = OnModel(initPeer, Chain.one(relay)).wrap(
      OnModel(otherPeer, Chain.one(otherRelay)).wrap(
        SeqModel.wrap(
          callModel(1),
          callModel(2)
        )
      )
    )

    val proc = Topology.resolve(init).value

    val expected =
      SeqRes.wrap(
        through(relay),
        through(otherRelay),
        callRes(1, otherPeer),
        callRes(2, otherPeer)
      )

    proc.equalsOrShowDiff(expected) should be(true)
  }

  "topology resolver" should "build return path in par if there are exported variables" in {
    val exportTo = CallModel.Export("result", ScalarType.string) :: Nil
    val result = VarRaw("result", ScalarType.string)

    val init = OnModel(initPeer, Chain.one(relay)).wrap(
      SeqModel.wrap(
        ParModel.wrap(
          OnModel(
            otherPeer,
            Chain.one(otherRelay)
          ).wrap(callModel(1, exportTo)),
          callModel(2)
        ),
        callModel(3, Nil, result :: Nil)
      )
    )

    val proc = Topology.resolve(init).value

    val expected =
      SeqRes.wrap(
        ParRes.wrap(
          SeqRes.wrap(
            through(relay),
            through(otherRelay),
            callRes(1, otherPeer, exportTo.headOption),
            through(otherRelay),
            through(relay),
            // we should return to a caller to continue execution
            through(initPeer)
          ),
          callRes(2, initPeer)
        ),
        callRes(3, initPeer, None, result :: Nil)
      )

    proc.equalsOrShowDiff(expected) should be(true)
  }

  "topology resolver" should "work fine with par" in {
    val init = OnModel(initPeer, Chain.one(relay)).wrap(
      ParModel.wrap(
        OnModel(
          otherPeer,
          Chain.one(otherRelay)
        ).wrap(callModel(1)),
        callModel(2)
      )
    )

    val proc = Topology.resolve(init).value

    val expected =
      ParRes.wrap(
        SeqRes.wrap(
          through(relay),
          through(otherRelay),
          callRes(1, otherPeer)
        ),
        callRes(2, initPeer)
      )

    proc.equalsOrShowDiff(expected) should be(true)
  }

  "topology resolver" should "create correct calls in try" in {
    val init = XorModel.wrap(callModel(1))

    val proc = Topology.resolve(init).value

    proc.equalsOrShowDiff(
      Cofree[Chain, ResolvedOp](XorRes, Eval.now(Chain.one(callRes(1, initPeer))))
    ) should be(true)
  }

  "topology resolver" should "work fine with par with on" in {
    val init = OnModel(initPeer, Chain.one(relay)).wrap(
      ParModel.wrap(
        OnModel(
          otherPeer,
          Chain.one(otherRelay)
        ).wrap(callModel(1)),
        OnModel(otherPeer2, Chain.one(otherRelay2)).wrap(
          callModel(2)
        )
      )
    )

    val proc = Topology.resolve(init).value

    val expected =
      ParRes.wrap(
        SeqRes.wrap(
          through(relay),
          through(otherRelay),
          callRes(1, otherPeer)
        ),
        SeqRes.wrap(
          through(relay),
          through(otherRelay2),
          callRes(2, otherPeer2)
        )
      )

    proc.equalsOrShowDiff(expected) should be(true)
  }

  "topology resolver" should "go through relay to any other node, via another relay, in complex xor/seq" in {

    val init = OnModel(initPeer, Chain.one(relay)).wrap(
      OnModel(otherPeer, Chain.one(otherRelay)).wrap(
        XorModel.wrap(
          SeqModel.wrap(
            callModel(1),
            callModel(2)
          ),
          callModel(3)
        )
      )
    )

    val proc = Topology.resolve(init).value

    val expected =
      SeqRes.wrap(
        through(relay),
        through(otherRelay),
        XorRes.wrap(
          SeqRes.wrap(
            callRes(1, otherPeer),
            callRes(2, otherPeer)
          ),
          callRes(3, otherPeer)
        )
      )

    proc.equalsOrShowDiff(expected) should be(true)
  }

  "topology resolver" should "simplify a route with init_peer_id" in {
    val init = OnModel(initPeer, Chain.one(relay)).wrap(
      SeqModel.wrap(
        OnModel(
          initPeer,
          Chain.one(relay)
        ).wrap(callModel(1)),
        callModel(2)
      )
    )

    val proc = Topology.resolve(init).value

    val expected =
      SeqRes.wrap(
        callRes(1, initPeer),
        callRes(2, initPeer)
      )

    proc.equalsOrShowDiff(expected) should be(true)
  }

  "topology resolver" should "get back to init peer" in {

    val init = OnModel(initPeer, Chain.one(relay)).wrap(
      SeqModel.wrap(
        OnModel(
          otherPeer,
          Chain.one(otherRelay)
        ).wrap(callModel(1)),
        callModel(2)
      )
    )

    val proc = Topology.resolve(init).value

    val expected =
      SeqRes.wrap(
        through(relay),
        through(otherRelay),
        callRes(1, otherPeer),
        through(otherRelay),
        through(relay),
        callRes(2, initPeer)
      )

    proc.equalsOrShowDiff(expected) should be(true)
  }

  "topology resolver" should "not stackoverflow" in {
    val init = OnModel(initPeer, Chain.one(relay)).wrap(
      SeqModel.wrap(
        callModel(1),
        callModel(2),
        callModel(3),
        OnModel(varNode, Chain.one(viaList)).wrap(
          callModel(4)
        ),
        OnModel(initPeer, Chain.one(relay)).wrap(
          callModel(5)
        )
      )
    )

    Topology.resolve(init).value
  }

  "topology resolver" should "get back to init peer after a long chain" in {
    val init = OnModel(initPeer, Chain.one(relay)).wrap(
      SeqModel.wrap(
        OnModel(otherPeer, Chain.one(otherRelay)).wrap(
          callModel(0),
          OnModel(otherPeer2, Chain.one(otherRelay)).wrap(
            callModel(1),
            MatchMismatchModel(otherPeer, otherRelay, true).wrap(
              OnModel(otherPeer, Chain.one(otherRelay)).wrap(
                callModel(2)
              )
            )
          )
        ),
        callModel(3)
      )
    )

    val proc = Topology.resolve(init).value

    val expected =
      SeqRes.wrap(
        through(relay),
        through(otherRelay),
        callRes(0, otherPeer),
        through(otherRelay),
        callRes(1, otherPeer2),
        MatchMismatchRes(otherPeer, otherRelay, true).wrap(
          SeqRes.wrap(
            through(otherRelay),
            callRes(2, otherPeer)
          )
        ),
        through(otherRelay),
        through(relay),
        callRes(3, initPeer)
      )

    proc.equalsOrShowDiff(expected) should be(true)
  }

  "topology resolver" should "resolve xor path" in {

    val init = OnModel(initPeer, Chain.one(relay)).wrap(
      SeqModel.wrap(
        XorModel.wrap(
          OnModel(otherPeer, Chain.one(otherRelay)).wrap(
            callModel(0)
          ),
          OnModel(initPeer, Chain.one(relay)).wrap(
            callModel(1)
          )
        ),
        OnModel(otherPeer, Chain.one(otherRelay)).wrap(
          callModel(3)
        ),
        callModel(4)
      )
    )

    val proc = Topology.resolve(init).value

    val expected =
      SeqRes.wrap(
        XorRes.wrap(
          SeqRes.wrap(
            through(relay),
            through(otherRelay),
            callRes(0, otherPeer)
          ),
          SeqRes.wrap(
            through(otherRelay),
            through(relay),
            callRes(1, initPeer),
            through(relay),
            through(otherRelay)
          )
        ),
        callRes(3, otherPeer),
        through(otherRelay),
        through(relay),
        callRes(4, initPeer)
      )

    proc.equalsOrShowDiff(expected) should be(true)
  }

  // func registerKeyPutValue(node_id: string) -> []string:
  //  on node_id:
  //    nodes <- OpH.pr()
  //  on node_id:
  //      for n <- nodes par:
  //        on n:
  //          OpH.op("in")
  //  <- nodes
  // this example doesn't create a hop on relay after fold
  // but the test create it, so there is not a one-on-one simulation
  // change it or write an integration test
  "topology resolver" should "create returning hops on chain of 'on'" in {
    val init =
      OnModel(initPeer, Chain.one(relay)).wrap(
        OnModel(otherPeer, Chain.empty).wrap(
          callModel(0)
        ),
        OnModel(otherPeer, Chain.empty).wrap(
          foldPar(
            "i",
            valueArray,
            OnModel(otherPeer2, Chain.empty).wrap(
              callModel(2)
            )
          )
        ),
        callModel(3)
      )

    val proc = Topology.resolve(init).value

    val expected =
      SeqRes.wrap(
        through(relay),
        callRes(0, otherPeer),
        ParRes.wrap(
          FoldRes("i", valueArray).wrap(ParRes.wrap(callRes(2, otherPeer2), NextRes("i").leaf))
        ),
        through(relay),
        callRes(3, initPeer)
      )
    proc.equalsOrShowDiff(expected) should be(true)
  }

  "topology resolver" should "create returning hops on nested 'on'" in {
    val init =
      OnModel(initPeer, Chain.one(relay)).wrap(
        callModel(0),
        OnModel(otherPeer, Chain.empty).wrap(
          callModel(1),
          fold(
            "i",
            valueArray,
            OnModel(otherPeer2, Chain.one(otherRelay2)).wrap(
              callModel(2)
            )
          )
        ),
        callModel(3)
      )

    val proc = Topology.resolve(init).value

    val expected =
      SeqRes.wrap(
        callRes(0, initPeer),
        through(relay),
        callRes(1, otherPeer),
        through(otherRelay2),
        FoldRes("i", valueArray).wrap(
          callRes(2, otherPeer2),
          NextRes("i").leaf
        ),
        through(otherRelay2),
        through(relay),
        callRes(3, initPeer)
      )

    proc.equalsOrShowDiff(expected) should be(true)
  }

  // https://github.com/fluencelabs/aqua/issues/205
  "topology resolver" should "optimize path over fold" in {
    val i = VarRaw("i", ScalarType.string)
    val init =
      OnModel(initPeer, Chain.one(relay)).wrap(
        fold(
          "i",
          valueArray,
          OnModel(i, Chain.one(otherRelay)).wrap(
            callModel(1)
          )
        )
      )

    val proc = Topology.resolve(init).value

    val expected =
      SeqRes.wrap(
        through(relay),
        FoldRes("i", valueArray).wrap(
          SeqRes.wrap(
            through(otherRelay),
            callRes(1, i)
          ),
          SeqRes.wrap(
            through(otherRelay),
            NextRes("i").leaf
          )
        )
      )

    proc.equalsOrShowDiff(expected) should be(true)
  }

  "topology resolver" should "handle detach" in {
    val init =
      OnModel(initPeer, Chain.one(relay)).wrap(
        DetachModel.wrap(
          OnModel(otherPeer, Chain.empty).wrap(
            callModel(1, CallModel.Export(varNode.name, varNode.baseType) :: Nil)
          )
        ),
        callModel(2, Nil, varNode :: Nil)
      )

    val proc = Topology.resolve(init).value

    val expected =
      SeqRes.wrap(
        ParRes.wrap(
          SeqRes.wrap(
            through(relay),
            callRes(1, otherPeer, Some(CallModel.Export(varNode.name, varNode.baseType))),
            through(relay),
            through(initPeer) // pingback
          )
        ),
        callRes(2, initPeer, None, varNode :: Nil)
      )

    proc.equalsOrShowDiff(expected) should be(true)
  }

  "topology resolver" should "handle moved detach" in {
    val init =
      OnModel(initPeer, Chain.one(relay)).wrap(
        OnModel(otherPeer2, Chain.empty).wrap(
          DetachModel.wrap(
            OnModel(otherPeer, Chain.empty).wrap(
              callModel(1, CallModel.Export(varNode.name, varNode.baseType) :: Nil)
            )
          ),
          callModel(2, Nil, varNode :: Nil)
        )
      )

    val proc = Topology.resolve(init).value

    val expected =
      SeqRes.wrap(
        through(relay),
        ParRes.wrap(
          SeqRes.wrap(
            callRes(1, otherPeer, Some(CallModel.Export(varNode.name, varNode.baseType))),
            through(otherPeer2) // pingback
          )
        ),
        callRes(2, otherPeer2, None, varNode :: Nil)
      )

    proc.equalsOrShowDiff(expected) should be(true)
  }

  "topology resolver" should "make right hops on for-par behaviour" in {
    val init = SeqModel.wrap(
      OnModel(otherPeer, Chain.one(relayV)).wrap(
        callModel(1),
        foldPar(
          "i",
          valueArray,
          OnModel(LiteralRaw("i", ScalarType.string), Chain.empty).wrap(
            callModel(2, CallModel.Export("used", StreamType(ScalarType.string)) :: Nil)
          )
        ),
        OnModel(otherPeer2, Chain.empty).wrap(
          callModel(3, Nil, VarRaw("used", StreamType(ScalarType.string)) :: Nil)
        )
      )
    )

    val proc = Topology.resolve(init).value

    val expected =
      SeqRes.wrap(
        callRes(1, otherPeer),
        ParRes.wrap(
          FoldRes("i", valueArray).wrap(
            ParRes.wrap(
              SeqRes.wrap(
                // TODO: should be outside of fold
                through(relayV),
                callRes(
                  2,
                  LiteralRaw("i", ScalarType.string),
                  Some(CallModel.Export("used", StreamType(ScalarType.string)))
                ),
                // after call `i` topology should send to `otherPeer2` if it's not fire-and-forget – to trigger execution
                through(otherPeer2)
              ),
              NextRes("i").leaf
            )
          )
        ),
        callRes(3, otherPeer2, None, VarModel("used", StreamType(ScalarType.string)) :: Nil)
      )

    proc.equalsOrShowDiff(expected) should be(true)

  }

  "topology resolver" should "handle detach moved to relay" in {
    val init =
      OnModel(initPeer, Chain.one(relay)).wrap(
        OnModel(relay, Chain.empty).wrap(
          DetachModel.wrap(
            OnModel(otherPeer, Chain.empty).wrap(
              callModel(1, CallModel.Export(varNode.name, varNode.baseType) :: Nil)
            )
          )
        ),
        callModel(2, Nil, varNode :: Nil)
      )

    val proc = Topology.resolve(init).value

    val expected =
      SeqRes.wrap(
        through(relay),
        ParRes.wrap(
          SeqRes.wrap(
            callRes(1, otherPeer, Some(CallModel.Export(varNode.name, varNode.baseType))),
            through(relay), // pingback
            through(initPeer) // pingback
          )
        ),
        callRes(2, initPeer, None, varNode :: Nil)
      )

    proc.equalsOrShowDiff(expected) should be(true)
  }

  "topology resolver" should "place ping inside par" in {
    val i = LiteralRaw("i", ScalarType.string)
    val used = VarRaw("used", StreamType(ScalarType.string))
    val usedWithIdx = used.copy(lambda =
      Chain.one(IntoIndexRaw(LiteralRaw("1", ScalarType.u32), ScalarType.string))
    )
    val init =
      OnModel(initPeer, Chain.one(relay)).wrap(
        foldPar(
          "i",
          valueArray,
          OnModel(i, Chain.empty).wrap(
            callModel(1, CallModel.Export(used.name, used.`type`) :: Nil)
          )
        ),
        JoinModel(NonEmptyList.one(usedWithIdx)).leaf,
        callModel(3, Nil, used :: Nil)
      )

    val proc = Topology.resolve(init).value

    val expected =
      SeqRes.wrap(
        ParRes.wrap(
          FoldRes("i", ValueModel.fromRaw(valueArray)).wrap(
            ParRes.wrap(
              SeqRes.wrap(
                through(relay),
                callRes(
                  1,
                  ValueModel.fromRaw(i),
                  Some(CallModel.Export(used.name, used.`type`))
                ),
                through(relay),
                through(initPeer)
              ),
              NextRes("i").leaf
            )
          )
        ),
        CallServiceRes(
          LiteralModel(s"\"op\"", LiteralType.string),
          s"noop",
          CallRes(usedWithIdx :: Nil, None),
          initPeer
        ).leaf,
        callRes(3, initPeer, None, ValueModel.fromRaw(used) :: Nil)
      )

    proc.equalsOrShowDiff(expected) should be(true)
  }

  "topology resolver" should "place ping inside par with xor" in {
    val i = LiteralRaw("i", ScalarType.string)
    val used = VarRaw("used", StreamType(ScalarType.string))
    val usedWithIdx = used.copy(lambda =
      Chain.one(IntoIndexRaw(LiteralRaw("1", ScalarType.u32), ScalarType.string))
    )
    val init =
      OnModel(initPeer, Chain.one(relay)).wrap(
        foldPar(
          "i",
          valueArray,
          OnModel(i, Chain.empty).wrap(
            XorModel.wrap(
              callModel(1, CallModel.Export(used.name, used.`type`) :: Nil)
            )
          )
        ),
        JoinModel(NonEmptyList.one(usedWithIdx)).leaf,
        callModel(3, Nil, used :: Nil)
      )

    val proc = Topology.resolve(init).value

    val expected =
      SeqRes.wrap(
        ParRes.wrap(
          FoldRes("i", ValueModel.fromRaw(valueArray)).wrap(
            ParRes.wrap(
              SeqRes.wrap(
                through(relay),
                XorRes.wrap(
                  callRes(
                    1,
                    ValueModel.fromRaw(i),
                    Some(CallModel.Export(used.name, used.`type`))
                  )
                ),
                through(relay),
                through(initPeer)
              ),
              NextRes("i").leaf
            )
          )
        ),
        CallServiceRes(
          LiteralModel(s"\"op\"", LiteralType.string),
          s"noop",
          CallRes(usedWithIdx :: Nil, None),
          initPeer
        ).leaf,
        callRes(3, initPeer, None, ValueModel.fromRaw(used) :: Nil)
      )

    proc.equalsOrShowDiff(expected) should be(true)
  }
}
