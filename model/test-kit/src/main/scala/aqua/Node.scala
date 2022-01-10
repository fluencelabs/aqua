package aqua

import aqua.model.func.raw.*
import aqua.model.transform.TransformConfig
import aqua.model.transform.res.{CallRes, CallServiceRes, MakeRes, MatchMismatchRes, NextRes, ResolvedOp}
import aqua.model.transform.funcop.ErrorsCatcher
import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.raw.ops
import aqua.raw.ops.{Call, CallServiceTag, ForTag, FuncOp, MatchMismatchTag, NextTag, OnTag, ParTag, RawTag, SeqTag, XorTag}
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
    ops.FuncOp(tree.cof)

  implicit def funcOpToRaw(op: FuncOp): Raw =
    op.tree

  val relay = LiteralModel("-relay-", ScalarType.string)
  val relayV = VarModel("-relay-", ScalarType.string)
  val initPeer = LiteralModel.initPeerId
  val emptyCall = Call(Nil, Nil)
  val otherPeer = LiteralModel("other-peer", ScalarType.string)
  val otherPeerL = LiteralModel("\"other-peer\"", LiteralType.string)
  val otherRelay = LiteralModel("other-relay", ScalarType.string)
  val otherPeer2 = LiteralModel("other-peer-2", ScalarType.string)
  val otherRelay2 = LiteralModel("other-relay-2", ScalarType.string)
  val varNode = VarModel("node-id", ScalarType.string)
  val viaList = VarModel("other-relay-2", ArrayType(ScalarType.string))
  val valueArray = VarModel("array", ArrayType(ScalarType.string))

  def callRes(
    i: Int,
    on: ValueModel,
    exportTo: Option[Call.Export] = None,
    args: List[ValueModel] = Nil
  ): Res = Node(
    CallServiceRes(LiteralModel(s"srv$i", ScalarType.string), s"fn$i", CallRes(args, exportTo), on)
  )

  def callTag(i: Int, exportTo: List[Call.Export] = Nil, args: List[ValueModel] = Nil): Raw =
    Node(
      CallServiceTag(LiteralModel(s"srv$i", ScalarType.string), s"fn$i", Call(args, exportTo))
    )

  def callLiteralRes(i: Int, on: ValueModel, exportTo: Option[Call.Export] = None): Res = Node(
    CallServiceRes(
      LiteralModel("\"srv" + i + "\"", LiteralType.string),
      s"fn$i",
      CallRes(Nil, exportTo),
      on
    )
  )

  def callLiteralRaw(i: Int, exportTo: List[Call.Export] = Nil): Raw = Node(
    ops.CallServiceTag(
      LiteralModel("\"srv" + i + "\"", LiteralType.string),
      s"fn$i",
      Call(Nil, exportTo)
    )
  )

  def errorCall(bc: TransformConfig, i: Int, on: ValueModel = initPeer): Res = Node[ResolvedOp](
    CallServiceRes(
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
      CallServiceRes(
        bc.callbackSrvId,
        bc.respFuncName,
        CallRes(value :: Nil, None),
        on
      )
    )

  def dataCall(bc: TransformConfig, name: String, on: ValueModel = initPeer): Res =
    Node[ResolvedOp](
      CallServiceRes(
        bc.dataSrvId,
        name,
        CallRes(Nil, Some(Call.Export(name, ScalarType.string))),
        on
      )
    )

  def fold(item: String, iter: ValueModel, body: Raw*) =
    Node(
      ForTag(item, iter),
      body.toList :+ next(item)
    )

  def foldPar(item: String, iter: ValueModel, body: Raw*) = {
    val ops = Node(SeqTag, body.toList)
    Node(
      ops.ForTag(item, iter),
      List(
        Node(ParTag, List(ops, next(item)))
      )
    )
  }

  def co(body: Raw*) = 
    Node(
      ParTag.Detach,
      body.toList
    )

  def on(peer: ValueModel, via: List[ValueModel], body: Raw*) =
    Node(
      OnTag(peer, Chain.fromSeq(via)),
      body.toList
    )

  def nextRes(item: String): Res =
    Node(NextRes(item))

  def next(item: String): Raw =
    Node(
      NextTag(item)
    )

  def `try`(body: Raw*) =
    Node(XorTag.LeftBiased, body.toList)

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
