package aqua.model

import aqua.model.body.{Call, CallServiceTag, FuncOp, OnTag, OpTag, SeqTag, XorTag}
import aqua.model.transform.BodyConfig
import aqua.types.{LiteralType, ScalarType, Type}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree

import scala.language.implicitConversions

// Helper to simplify building and visualizing Cofree structures
case class Node(tag: OpTag, ops: List[Node] = Nil) {

  override def toString: String =
    tag.toString + (if (ops.isEmpty) "\n" else s"{\n${ops.mkString}\n}\n")

  def equalOrNot[T](left: T, right: T): String = (if (left == right)
                                                    Console.GREEN + left + Console.RESET
                                                  else
                                                    Console.BLUE + left + Console.RED + " != " + Console.YELLOW + right)

  def diffArg(left: (ValueModel, Type), right: (ValueModel, Type)): String =
    Console.GREEN + "(" +
      equalOrNot(left._1, right._1) + Console.GREEN + ", " +
      equalOrNot(left._2, right._2) + Console.GREEN + ")"

  def diffCall(left: Call, right: Call): String =
    if (left == right) Console.GREEN + left + Console.RESET
    else
      Console.GREEN + "Call(" +
        equalOrNot(left.args.length, right.args.length) + "::" + left.args
          .zip(right.args)
          .map(ab => diffArg(ab._1, ab._2))
          .mkString("::") + Console.GREEN + ", " +
        equalOrNot(left.exportTo, right.exportTo) + Console.GREEN + ")"

  def diffServiceCall(left: CallServiceTag, right: CallServiceTag): String =
    Console.GREEN + "CallServiceTag(" +
      equalOrNot(left.serviceId, right.serviceId) + Console.GREEN + ", " +
      equalOrNot(left.funcName, right.funcName) + Console.GREEN + ", " +
      diffCall(left.call, right.call) + Console.GREEN + ", " +
      equalOrNot(left.peerId, right.peerId) +
      Console.GREEN + ")" + Console.RESET

  def diffTags(left: OpTag, right: OpTag): String = (left, right) match {
    case (l: CallServiceTag, r: CallServiceTag) => diffServiceCall(l, r)
    case _ =>
      Console.BLUE + s"    $left ${Console.RED}\n != ${Console.YELLOW}${right}${Console.RED}"
  }

  def diffToString(other: Node): String =
    (if (tag == other.tag) Console.GREEN + tag
     else diffTags(tag, other.tag)) + (if (ops.isEmpty && other.ops.isEmpty) "\n"
                                       else
                                         "{\n") + Console.RESET +
      (if (ops.length != other.ops.length)
         Console.RED + s"number of ops: ${ops.length} != ${other.ops.length}\n" + Console.RESET
       else "") +
      ops
        .zip(other.ops)
        .map { case (a, b) =>
          a.diffToString(b)
        }
        .mkString + (if (ops.isEmpty && other.ops.isEmpty) ""
                     else
                       ((if (tag == other.tag) Console.GREEN
                         else Console.RED) + "}\n" + Console.RESET))

}

object Node {
  type Cof = Cofree[Chain, OpTag]

  implicit def cofToNode(cof: Cof): Node =
    Node(cof.head, cof.tailForced.toList.map(cofToNode))

  implicit def nodeToCof(tree: Node): Cof =
    Cofree(tree.tag, Eval.later(Chain.fromSeq(tree.ops.map(nodeToCof))))

  val relay = LiteralModel("relay")
  val relayV = VarModel("relay")
  val initPeer = InitPeerIdModel
  val emptyCall = Call(Nil, None)
  val otherPeer = LiteralModel("other-peer")
  val otherRelay = LiteralModel("other-relay")
  val otherPeer2 = LiteralModel("other-peer-2")
  val otherRelay2 = LiteralModel("other-relay-2")

  def call(i: Int, on: ValueModel = null) = Node(
    CallServiceTag(LiteralModel(s"srv$i"), s"fn$i", Call(Nil, None), Option(on))
  )

  def xorErrorCall(bc: BodyConfig, on: ValueModel = null) = Node(
    CallServiceTag(
      bc.errorHandlingCallback,
      bc.errorFuncName,
      Call((LiteralModel("%last_error%"), ScalarType.string) :: Nil, None),
      Option(on)
    )
  )

  def respCall(bc: BodyConfig, value: ValueModel, on: ValueModel = null) = Node(
    CallServiceTag(
      bc.callbackSrvId,
      bc.respFuncName,
      Call((value, ScalarType.string) :: Nil, None),
      Option(on)
    )
  )

  def dataCall(bc: BodyConfig, name: String, on: ValueModel = null) = Node(
    CallServiceTag(
      bc.dataSrvId,
      name,
      Call(Nil, Some(name)),
      Option(on)
    )
  )

  def seq(nodes: Node*) = Node(SeqTag, nodes.toList)
  def xor(left: Node, right: Node) = Node(XorTag, left :: right :: Nil)

  def on(peer: ValueModel, via: List[ValueModel], body: Node*) =
    Node(
      OnTag(peer, Chain.fromSeq(via)),
      body.toList
    )

  def through(peer: ValueModel): Node =
    FuncOp.noop(peer).tree
}
