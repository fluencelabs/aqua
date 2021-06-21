package aqua

import aqua.model.func.Call
import aqua.model.func.raw._
import aqua.model.func.resolved.{CallServiceRes, MakeRes, MatchMismatchRes, ResolvedOp}
import aqua.model.transform.{BodyConfig, ErrorsCatcher}
import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.types.{ArrayType, LiteralType, ScalarType}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree

import scala.language.implicitConversions

// Helper to simplify building and visualizing Cofree structures
case class Node[+T](label: T, children: List[Node[T]] = Nil) {

  def cof[TT >: T]: Cofree[Chain, TT] = Node.nodeToCof(this)

  override def toString: String =
    label.toString + (if (children.isEmpty) "\n" else s"{\n${children.mkString}\n}\n")

  def equalsOrPrintDiff[TT](other: Node[TT]): Boolean =
    if (this == other) true
    else {
      println(Console.CYAN + "Given: " + this)
      println(Console.YELLOW + "Other: " + other + Console.RESET)
      false
    }
}

object Node {
  type Res = Node[ResolvedOp]
  type Raw = Node[RawTag]

  implicit def cofToNode[T](cof: Cofree[Chain, T]): Node[T] =
    Node[T](cof.head, cof.tailForced.toList.map(cofToNode[T]))

  implicit def nodeToCof[T](tree: Node[T]): Cofree[Chain, T] =
    Cofree(tree.label, Eval.later(Chain.fromSeq(tree.children.map(nodeToCof))))

  implicit def rawToFuncOp(tree: Raw): FuncOp =
    FuncOp(tree.cof)

  implicit def funcOpToRaw(op: FuncOp): Raw =
    op.tree

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

  def callRes(i: Int, on: ValueModel): Res = Node(
    CallServiceRes(LiteralModel(s"srv$i", ScalarType.string), s"fn$i", Call(Nil, None), on)
  )

  def callTag(i: Int): Raw = Node(
    CallServiceTag(LiteralModel(s"srv$i", ScalarType.string), s"fn$i", Call(Nil, None))
  )

  def callLiteralRes(i: Int, on: ValueModel): Res = Node(
    CallServiceRes(
      LiteralModel("\"srv" + i + "\"", LiteralType.string),
      s"fn$i",
      Call(Nil, None),
      on
    )
  )

  def callLiteralRaw(i: Int): Raw = Node(
    CallServiceTag(
      LiteralModel("\"srv" + i + "\"", LiteralType.string),
      s"fn$i",
      Call(Nil, None)
    )
  )

  def errorCall(bc: BodyConfig, i: Int, on: ValueModel = initPeer): Res = Node[ResolvedOp](
    CallServiceRes(
      bc.errorHandlingCallback,
      bc.errorFuncName,
      Call(
        ErrorsCatcher.lastErrorArg :: LiteralModel(
          i.toString,
          LiteralType.number
        ) :: Nil,
        None
      ),
      on
    )
  )

  def respCall(bc: BodyConfig, value: ValueModel, on: ValueModel = initPeer): Res =
    Node[ResolvedOp](
      CallServiceRes(
        bc.callbackSrvId,
        bc.respFuncName,
        Call(value :: Nil, None),
        on
      )
    )

  def dataCall(bc: BodyConfig, name: String, on: ValueModel = initPeer): Res = Node[ResolvedOp](
    CallServiceRes(
      bc.dataSrvId,
      name,
      Call(Nil, Some(Call.Export(name, ScalarType.string))),
      on
    )
  )

  def on(peer: ValueModel, via: List[ValueModel], body: Raw*) =
    Node(
      OnTag(peer, Chain.fromSeq(via)),
      body.toList
    )

  def matchRes(l: ValueModel, r: ValueModel, body: Res): Res =
    Node(
      MatchMismatchRes(l, r, shouldMatch = true),
      body :: Nil
    )

  def matchRaw(l: ValueModel, r: ValueModel, body: Raw): Raw =
    Node(
      MatchMismatchTag(l, r, shouldMatch = true),
      body :: Nil
    )

  def through(peer: ValueModel): Node[ResolvedOp] =
    cofToNode(MakeRes.noop(peer))

  private def equalOrNot[TT](left: TT, right: TT): String = (if (left == right)
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

  private def diffServiceCall(left: CallServiceRes, right: CallServiceRes): String =
    Console.GREEN + "CallServiceTag(" +
      equalOrNot(left.serviceId, right.serviceId) + Console.GREEN + ", " +
      equalOrNot(left.funcName, right.funcName) + Console.GREEN + ", " +
      diffCall(left.call, right.call) + Console.GREEN +
      Console.GREEN + ")" + Console.RESET

  private def diffRes(left: ResolvedOp, right: ResolvedOp): String = (left, right) match {
    case (l: CallServiceRes, r: CallServiceRes) => diffServiceCall(l, r)
    case _ =>
      Console.BLUE + s"    $left ${Console.RED}\n != ${Console.YELLOW}${right}${Console.RED}"
  }

  def diffToString(current: Node.Res, other: Node.Res): String =
    (if (current.label == other.label) Console.GREEN + current.label
     else
       diffRes(current.label, other.label)) + (if (
                                                 current.children.isEmpty && other.children.isEmpty
                                               ) "\n"
                                               else
                                                 "{\n") + Console.RESET +
      (if (current.children.length != other.children.length)
         Console.RED + s"number of ops: ${current.children.length} != ${other.children.length}\n" + Console.RESET
       else "") +
      current.children
        .zip(other.children)
        .map { case (a, b) =>
          diffToString(a, b)
        }
        .mkString + (if (current.children.isEmpty && other.children.isEmpty) ""
                     else
                       ((if (current.label == other.label) Console.GREEN
                         else Console.RED) + "}\n" + Console.RESET))

  def equalsOrPrintDiff(current: Node.Res, other: Node.Res): Boolean =
    if (current == other) true
    else {
      println(diffToString(current, other))
      false
    }
}
