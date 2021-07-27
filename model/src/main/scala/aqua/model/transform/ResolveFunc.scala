package aqua.model.transform

import aqua.model.func._
import aqua.model.func.raw.{FuncOp, FuncOps}
import aqua.model.{ValueModel, VarModel}
import aqua.types.{ArrayType, ArrowType, StreamType}
import cats.Eval
import cats.syntax.apply._

case class ResolveFunc(
  transform: FuncOp => FuncOp,
  callback: (String, Call) => FuncOp,
  respFuncName: String,
  wrapCallableName: String = "funcAround",
  arrowCallbackPrefix: String = "init_peer_callable_"
) {

  private val returnVar: String = "-return-"

  def returnCallback(retModel: ValueModel): FuncOp =
    callback(
      respFuncName,
      Call(
        retModel :: Nil,
        None
      )
    )

  def arrowToCallback(name: String, arrowType: ArrowType): FuncCallable = {
    val (args, call, ret) = ArgsCall.arrowToArgsCallRet(arrowType)
    FuncCallable(
      arrowCallbackPrefix + name,
      callback(name, call),
      args,
      (ret.map(_.model), arrowType.res).mapN(_ -> _),
      Map.empty,
      Map.empty
    )
  }

  def wrap(func: FuncCallable): FuncCallable = {
    val returnType = func.ret.map(_._1.lastType).map {
      // we mustn't return a stream in response callback to avoid pushing stream to `-return-` value
      case StreamType(t) => ArrayType(t)
      case t => t
    }

    FuncCallable(
      wrapCallableName,
      transform(
        FuncOps.seq(
          FuncOps
            .callArrow(
              func.funcName,
              Call(
                func.args.toCallArgs,
                returnType.map(t => Call.Export(returnVar, t))
              )
            ) ::
            returnType
              .map(t => VarModel(returnVar, t))
              .map(returnCallback)
              .toList: _*
        )
      ),
      ArgsDef(ArgDef.Arrow(func.funcName, func.arrowType) :: Nil),
      None,
      func.args.arrowArgs.map { case ArgDef.Arrow(argName, arrowType) =>
        argName -> arrowToCallback(argName, arrowType)
      }.toList.toMap,
      Map.empty
    )
  }

  def resolve(
    func: FuncCallable,
    funcArgName: String = "_func"
  ): Eval[FuncOp] =
    wrap(func)
      .resolve(
        Call(VarModel(funcArgName, func.arrowType) :: Nil, None),
        Map(funcArgName -> func),
        Set.empty
      )
      .map(_._1)
}
