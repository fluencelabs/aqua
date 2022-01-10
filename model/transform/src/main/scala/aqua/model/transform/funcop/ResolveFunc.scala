package aqua.model.transform.funcop

import aqua.model.func.*
import aqua.raw.ops.FuncOps
import aqua.raw.ops.{Call, FuncOp, FuncOps}
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.raw.arrow.{ArgsCall, Func}
import aqua.types.*
import cats.Eval

// TODO: doc
case class ResolveFunc(
  transform: FuncOp => FuncOp,
  callback: (String, Call) => FuncOp,
  respFuncName: String,
  wrapCallableName: String = "funcAround",
  arrowCallbackPrefix: String = "init_peer_callable_"
) {

  private val returnVar: String = "-return-"

  // TODO: doc
  def returnCallback(retModel: List[ValueRaw]): FuncOp =
    callback(
      respFuncName,
      Call(
        retModel,
        Nil
      )
    )

  // TODO: doc
  def arrowToCallback(name: String, arrowType: ArrowType): Func = {
    val (args, call, ret) = ArgsCall.arrowToArgsCallRet(arrowType)
    Func(
      arrowCallbackPrefix + name,
      callback(name, call),
      arrowType,
      ret.map(_.model),
      Map.empty,
      Map.empty
    )
  }

  // TODO: doc/rename
  def wrap(func: Func): Func = {
    val returnType = ProductType(func.ret.map(_.lastType).map {
      // we mustn't return a stream in response callback to avoid pushing stream to `-return-` value
      case StreamType(t) => ArrayType(t)
      case t => t
    }).toLabelledList(returnVar)

    val retModel = returnType.map { case (l, t) => VarRaw(l, t) }

    val funcCall = Call(
      func.arrowType.domain.toLabelledList().map(ad => VarRaw(ad._1, ad._2)),
      returnType.map { case (l, t) => Call.Export(l, t) }
    )

    Func(
      wrapCallableName,
      transform(
        FuncOps.seq(
          FuncOps
            .callArrow(
              func.funcName,
              funcCall
            ) :: (returnType.headOption
            .map(_ => returnCallback(retModel))
            .toList): _*
        )
      ),
      ArrowType(ConsType.cons(func.funcName, func.arrowType, NilType), NilType),
      Nil,
      func.arrowType.domain
        .toLabelledList()
        .collect { case (argName, arrowType: ArrowType) =>
          argName -> arrowToCallback(argName, arrowType)
        }
        .toMap,
      Map.empty
    )
  }

  // TODO: doc/rename
  def resolve(
    func: Func,
    funcArgName: String = "_func"
  ): Eval[FuncOp] =
    FuncResolver
      .resolve(
        wrap(func),
        Call(VarRaw(funcArgName, func.arrowType) :: Nil, Nil),
        Map(funcArgName -> func),
        Set.empty
      )
      .map(_._1)
}
