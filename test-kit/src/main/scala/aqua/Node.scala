package aqua

import aqua.model.func.Call
import aqua.model.func.raw._
import aqua.model.transform.{BodyConfig, ErrorsCatcher}
import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.types.{ArrayType, LiteralType, ScalarType}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree

import scala.language.implicitConversions

// Helper to simplify building and visualizing Cofree structures
case class Node(tag: RawTag, ops: List[Node] = Nil) {

  override def toString: String =
    tag.toString + (if (ops.isEmpty) "\n" else s"{\n${ops.mkString}\n}\n")

  private def equalOrNot[T](left: T, right: T): String = (if (left == right)
                                                            Console.GREEN + left + Console.RESET
                                                          else
                                                            Console.BLUE + left + Console.RED + " != " + Console.YELLOW + right)

  private def diffArg(left: ValueModel, right: ValueModel): String =
    Console.GREEN + "(" +
      equalOrNot(left, right) + Console.GREEN + ")"

  private def diffCall(left: Call, right: Call): String =
    if (left == right) Console.GREEN + left + Console.RESET
    else
      Console.GREEN + "Call(" +
        equalOrNot(left.args.length, right.args.length) + "::" + left.args
          .zip(right.args)
          .map(ab => diffArg(ab._1, ab._2))
          .mkString("::") + Console.GREEN + ", " +
        equalOrNot(left.exportTo, right.exportTo) + Console.GREEN + ")"

  private def diffServiceCall(left: CallServiceTag, right: CallServiceTag): String =
    Console.GREEN + "CallServiceTag(" +
      equalOrNot(left.serviceId, right.serviceId) + Console.GREEN + ", " +
      equalOrNot(left.funcName, right.funcName) + Console.GREEN + ", " +
      diffCall(left.call, right.call) + Console.GREEN + ", " +
      equalOrNot(left.peerId, right.peerId) +
      Console.GREEN + ")" + Console.RESET

  private def diffTags(left: RawTag, right: RawTag): String = (left, right) match {
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

  def equalsOrPrintDiff(other: Node): Boolean =
    if (this == other) true
    else {
      println(diffToString(other))
      false
    }
}

object Node {
  type Cof = Cofree[Chain, RawTag]

  implicit def cofToNode(cof: Cof): Node =
    Node(cof.head, cof.tailForced.toList.map(cofToNode))

  implicit def nodeToCof(tree: Node): Cof =
    Cofree(tree.tag, Eval.later(Chain.fromSeq(tree.ops.map(nodeToCof))))

  val relay = LiteralModel("-relay-", ScalarType.string)
  val relayV = VarModel("-relay-", ScalarType.string)
  val initPeer = LiteralModel.initPeerId
  val emptyCall = Call(Nil, None)
  val otherPeer = LiteralModel("other-peer", ScalarType.string)
  val otherRelay = LiteralModel("other-relay", ScalarType.string)
  val otherPeer2 = LiteralModel("other-peer-2", ScalarType.string)
  val otherRelay2 = LiteralModel("other-relay-2", ScalarType.string)
  val varNode = VarModel("node-id", ScalarType.string)
  val viaList = VarModel("other-relay-2", ArrayType(ScalarType.string))

  def call(i: Int, on: ValueModel = null) = Node(
    CallServiceTag(LiteralModel(s"srv$i", ScalarType.string), s"fn$i", Call(Nil, None), Option(on))
  )

  def callLiteral(i: Int, on: ValueModel = null) = Node(
    CallServiceTag(
      LiteralModel("\"srv" + i + "\"", LiteralType.string),
      s"fn$i",
      Call(Nil, None),
      Option(on)
    )
  )

  def errorCall(bc: BodyConfig, i: Int, on: ValueModel = null) = Node(
    CallServiceTag(
      bc.errorHandlingCallback,
      bc.errorFuncName,
      Call(
        ErrorsCatcher.lastErrorArg :: LiteralModel(
          i.toString,
          LiteralType.number
        ) :: Nil,
        None
      ),
      Option(on)
    )
  )

  def respCall(bc: BodyConfig, value: ValueModel, on: ValueModel = null) = Node(
    CallServiceTag(
      bc.callbackSrvId,
      bc.respFuncName,
      Call(value :: Nil, None),
      Option(on)
    )
  )

  def dataCall(bc: BodyConfig, name: String, on: ValueModel = null) = Node(
    CallServiceTag(
      bc.dataSrvId,
      name,
      Call(Nil, Some(Call.Export(name, ScalarType.string))),
      Option(on)
    )
  )

  def seq(nodes: Node*) = Node(SeqTag, nodes.toList)
  def xor(left: Node, right: Node) = Node(XorTag, left :: right :: Nil)

  def par(left: Node, right: Node) = Node(ParTag, left :: right :: Nil)

  def on(peer: ValueModel, via: List[ValueModel], body: Node*) =
    Node(
      OnTag(peer, Chain.fromSeq(via)),
      body.toList
    )

  def _match(l: ValueModel, r: ValueModel, body: Node) =
    Node(
      MatchMismatchTag(l, r, shouldMatch = true),
      body :: Nil
    )

  def through(peer: ValueModel): Node =
    FuncOps.noop(peer).tree
}
