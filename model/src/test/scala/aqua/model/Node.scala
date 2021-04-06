package aqua.model

import aqua.model.body.{Call, CallServiceTag, OnTag, OpTag, SeqTag}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree

import scala.language.implicitConversions

// Helper to simplify building and visualizing Cofree structures
case class Node(tag: OpTag, ops: List[Node] = Nil) {

  override def toString: String =
    tag.toString + (if (ops.isEmpty) "\n" else s"{\n${ops.mkString}\n}\n")
}

object Node {
  type Cof = Cofree[Chain, OpTag]

  implicit def cofToNode(cof: Cof): Node =
    Node(cof.head, cof.tailForced.toList.map(cofToNode))

  implicit def nodeToCof(tree: Node): Cof =
    Cofree(tree.tag, Eval.later(Chain.fromSeq(tree.ops.map(nodeToCof))))

  val relay = LiteralModel("relay")
  val initPeer = InitPeerIdModel
  val emptyCall = Call(Nil, None)
  val otherPeer = LiteralModel("other-peer")
  val otherRelay = LiteralModel("other-relay")

  def call(i: Int, on: ValueModel = null) = Node(
    CallServiceTag(LiteralModel(s"srv$i"), s"fn$i", Call(Nil, None), Option(on))
  )
  def seq(nodes: Node*) = Node(SeqTag, nodes.toList)

  def on(peer: ValueModel, via: List[ValueModel], body: Node*) =
    Node(
      OnTag(peer, via),
      body.toList
    )
}
