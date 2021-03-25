package aqua.model

import aqua.semantics.{ArrowType, DataType, Type}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree

case class FuncCallable(
  body: FuncOp,
  args: List[(String, Either[DataType, ArrowType])],
  ret: Option[(ValueModel, Type)],
  capturedArrows: Map[String, FuncCallable]
) {

  def apply(
    call: Call,
    arrows: Map[String, FuncCallable],
    forbiddenNames: Set[String]
  ): Eval[(FuncOp, Option[ValueModel])] = {

    val argsFull = args.zip(call.args)
    val argsToData = argsFull.collect { case ((n, Left(_)), v) =>
      n -> v._1
    }.toMap

    val argsToArrows = argsFull.collect { case ((n, Right(_)), (VarModel(name, _), _)) =>
      n -> arrows(name)
    }.toMap

    // Okay, now need to substitute calls
    val treeWithValues = body.resolveValues(argsToData).tree

    FuncOp
      .traverseA(treeWithValues, (forbiddenNames, Map.empty[String, String])) {
        case ((noNames, rename), CoalgebraTag(None, fn, c)) if argsToArrows.contains(fn) =>
          (noNames, rename) -> argsToArrows(fn).apply(c, argsToArrows, noNames).value._1.tree
        case (acc, tag) => acc -> Cofree[Chain, OpTag](tag, Eval.now(Chain.empty))
      }
      .map(_._2)
      .map(FuncOp(_))
      .map(_ -> ret.map(_._1))
  }

  val getDataService: String = "getDataSrv"
  val callbackService: String = "callbackSrv"

  val respFuncName = "response"
  val relayVarName = "relay"

  val returnCallback: Option[FuncOp] = ret.map { case (dv, t) =>
    viaRelay(
      FuncOp.leaf(
        CallServiceTag(
          LiteralModel("\"" + callbackService + "\""),
          respFuncName,
          Call(
            (dv, t) :: Nil,
            None
          )
        )
      )
    )
  }

  // TODO it's an overkill
  def initPeerCallable(name: String, arrowType: ArrowType): FuncCallable =
    FuncCallable(
      viaRelay(
        FuncOp.leaf(
          CallServiceTag(
            LiteralModel("\"" + callbackService + "\""),
            name,
            Call(
              arrowType.args.zipWithIndex.map { case (t, i) =>
                VarModel(s"arg$i") -> t
              },
              arrowType.res.map(_ => "init_call_res")
            )
          )
        )
      ),
      arrowType.args.zipWithIndex.map {
        case (t: DataType, i) => s"arg$i" -> Left(t)
        case (t: ArrowType, i) => s"arg$i" -> Right(t)
      },
      arrowType.res.map(VarModel("init_call_res") -> _),
      Map.empty
    )

  // TODO rename
  def generateTsModel: FuncOp = FuncOp.node(
    SeqTag,
    Chain
      .fromSeq(
        args.collect { case (argName, Left(_)) =>
          getDataOp(argName)
        } :+ getDataOp(relayVarName)
      )
      .append(
        apply(
          generateTsCall,
          args.collect { case (argName, Right(arrowType)) =>
            argName -> initPeerCallable(argName, arrowType)
          }.toMap,
          Set.empty
        ).value._1
      ) ++ Chain.fromSeq(returnCallback.toSeq)
  )

  def generateTsCall: Call =
    Call(
      args.map { case (k, e) =>
        (VarModel(k), e.fold(identity, identity))
      },
      None
    )

  def getDataOp(name: String): FuncOp =
    FuncOp.leaf(
      CallServiceTag(
        LiteralModel("\"" + getDataService + "\""),
        name,
        Call(Nil, Some(name))
      )
    )

  def viaRelay(op: FuncOp): FuncOp =
    FuncOp.node(
      OnTag(VarModel(relayVarName)),
      Chain(
        FuncOp.leaf(
          CallServiceTag(
            LiteralModel("\"op\""),
            "identity",
            Call(Nil, None)
          )
        ),
        FuncOp.wrap(OnTag(InitPeerIdModel), op)
      )
    )

}
