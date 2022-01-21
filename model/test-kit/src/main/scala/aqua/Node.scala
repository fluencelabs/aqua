package aqua

import aqua.model.transform.TransformConfig
import aqua.model.*
import aqua.model.transform.funcop.ErrorsCatcher
import aqua.raw.ops.*
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.res.{CallRes, CallServiceRes, MakeRes, MatchMismatchRes, NextRes, ResolvedOp}
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
  type Op = Node[OpModel]

  implicit def cofToNode[T](cof: Cofree[Chain, T]): Node[T] =
    Node[T](cof.head, cof.tailForced.toList.map(cofToNode[T]))

  implicit def nodeToCof[T](tree: Node[T]): Cofree[Chain, T] =
    Cofree(tree.label, Eval.later(Chain.fromSeq(tree.children.map(nodeToCof))))

  def seq(ops: Node.Op*): Node.Op = {
    if (ops.length == 1) ops.head
    else
      Node(
        SeqModel,
        ops.toList
      )
  }

  def par(ops: Node.Op*): Node.Op = {
    if (ops.length == 1) ops.head
    else
      Node(
        ParModel,
        ops.toList
      )
  }

  def xor(left: Node.Op, right: Node.Op): Node.Op = {
    Node(
      XorModel,
      left :: right :: Nil
    )
  }

  implicit def rawToValue(raw: ValueRaw): ValueModel = ValueModel.fromRaw(raw)

  val relay = LiteralRaw("-relay-", ScalarType.string)

  val relayV = VarRaw("-relay-", ScalarType.string)

  val initPeer = ValueRaw.InitPeerId

  val emptyCall = Call(Nil, Nil)

  val otherPeer = VarRaw("other-peer", ScalarType.string)

  val otherPeerL = LiteralRaw("\"other-peer\"", LiteralType.string)
  val otherRelay = LiteralRaw("other-relay", ScalarType.string)
  val otherPeer2 = LiteralRaw("other-peer-2", ScalarType.string)
  val otherRelay2 = LiteralRaw("other-relay-2", ScalarType.string)
  val varNode = VarRaw("node-id", ScalarType.string)
  val viaList = VarRaw("other-relay-2", ArrayType(ScalarType.string))
  val valueArray = VarRaw("array", ArrayType(ScalarType.string))

  def callRes(
    i: Int,
    on: ValueModel,
    exportTo: Option[model.CallModel.Export] = None,
    args: List[ValueModel] = Nil
  ): Res = Node(
    CallServiceRes(VarModel(s"srv$i", ScalarType.string), s"fn$i", CallRes(args, exportTo), on)
  )

  def callModel(i: Int, exportTo: List[CallModel.Export] = Nil, args: List[ValueRaw] = Nil): Op =
    Node(
      CallServiceModel(
        VarRaw(s"srv$i", ScalarType.string),
        s"fn$i",
        CallModel(args.map(ValueModel.fromRaw), exportTo)
      )
    )

  def callLiteralRes(i: Int, on: ValueModel, exportTo: Option[CallModel.Export] = None): Res = Node(
    res.CallServiceRes(
      LiteralModel("\"srv" + i + "\"", LiteralType.string),
      s"fn$i",
      CallRes(Nil, exportTo),
      on
    )
  )

  def callLiteralRaw(i: Int, exportTo: List[CallModel.Export] = Nil): Op = Node(
    CallServiceModel(
      LiteralRaw.quote("srv" + i),
      s"fn$i",
      CallModel(Nil, exportTo)
    )
  )

  def errorCall(bc: TransformConfig, i: Int, on: ValueModel = initPeer): Res = Node[ResolvedOp](
    res.CallServiceRes(
      bc.errorHandlingCallback,
      bc.errorFuncName,
      CallRes(
        ErrorsCatcher.lastErrorArg :: LiteralModel(
          i.toString,
          LiteralType.number
        ) :: Nil,
        None
      ),
      on
    )
  )

  def respCall(bc: TransformConfig, value: ValueModel, on: ValueModel = initPeer): Res =
    Node[ResolvedOp](
      res.CallServiceRes(
        ValueModel.fromRaw(bc.callbackSrvId),
        bc.respFuncName,
        CallRes(value :: Nil, None),
        on
      )
    )

  def dataCall(bc: TransformConfig, name: String, on: ValueModel = initPeer): Res =
    Node[ResolvedOp](
      res.CallServiceRes(
        ValueModel.fromRaw(bc.dataSrvId),
        name,
        CallRes(Nil, Some(CallModel.Export(name, ScalarType.string))),
        on
      )
    )

  def fold(item: String, iter: ValueRaw, body: Op*) =
    Node(
      ForModel(item, ValueModel.fromRaw(iter)),
      body.toList :+ next(item)
    )

  def foldPar(item: String, iter: ValueRaw, body: Op*) = {
    val ops = Node(SeqModel, body.toList)
    Node(
      ForModel(item, ValueModel.fromRaw(iter)),
      List(
        Node(ParModel, List(ops, next(item)))
      )
    )
  }

  def co(body: Op*) =
    Node(
      DetachModel,
      body.toList
    )

  def on(peer: ValueRaw, via: List[ValueRaw], body: Op*) =
    Node(
      OnModel(ValueModel.fromRaw(peer), Chain.fromSeq(via.map(ValueModel.fromRaw))),
      body.toList
    )

  def nextRes(item: String): Res =
    Node(NextRes(item))

  def next(item: String): Op =
    Node(
      NextModel(item)
    )

  def `try`(body: Op*) =
    Node(XorModel, body.toList)

  def matchRes(l: ValueModel, r: ValueModel, body: Res): Res =
    Node(
      MatchMismatchRes(l, r, shouldMatch = true),
      body :: Nil
    )

  def matchRaw(l: ValueRaw, r: ValueRaw, body: Op): Op =
    Node(
      MatchMismatchModel(ValueModel.fromRaw(l), ValueModel.fromRaw(r), shouldMatch = true),
      body :: Nil
    )

  def through(peer: ValueModel): Node[ResolvedOp] =
    cofToNode(MakeRes.noop(peer))

  private def equalOrNot[TT](left: TT, right: TT): String =
    (if (left == right)
       Console.GREEN + left + Console.RESET
     else
       Console.BLUE + left + Console.RED + " != " + Console.YELLOW + right)

  private def diffArg(left: ValueModel, right: ValueModel): String =
    Console.GREEN + "(" +
      equalOrNot(left, right) + Console.GREEN + ")"

  private def diffCall(left: CallRes, right: CallRes): String =
    if (left == right) Console.GREEN + left + Console.RESET
    else
      Console.GREEN + "Call(" +
        equalOrNot(left.args.length, right.args.length) + "::" + left.args
          .zip(right.args)
          .map(ab => diffArg(ab._1, ab._2))
          .mkString("::") + Console.GREEN + ", " +
        equalOrNot(left.exportTo, right.exportTo) + Console.GREEN + ")"

  private def diffServiceCall(left: CallServiceRes, right: CallServiceRes): String =
    Console.GREEN + "(call" +
      equalOrNot(left.peerId, right.peerId) + Console.GREEN + " (" +
      equalOrNot(left.serviceId, right.serviceId) + Console.GREEN + " " +
      equalOrNot(left.funcName, right.funcName) + Console.GREEN + ") " +
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
        .map(Option(_))
        .zipAll(other.children.map(Option(_)), None, None)
        .map {
          case (Some(a), Some(b)) =>
            diffToString(a, b)
          case (Some(a), _) =>
            Console.BLUE + a + Console.RESET
          case (_, Some(b)) =>
            Console.YELLOW + b + Console.RESET
          case _ =>
            Console.RED + "???" + Console.RESET
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
