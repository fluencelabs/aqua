package aqua.model.transform.funcop

import aqua.model.func.*
import aqua.model.func.raw.{FuncOp, FuncOps}
import aqua.model.{ValueModel, VarModel}
import aqua.types.{ArrayType, ArrowType, ConsType, NilType, ProductType, StreamType}
import cats.Eval

case class ResolveFunc(
  transform: FuncOp => FuncOp,
  callback: (String, Call) => FuncOp,
  respFuncName: String,
  wrapCallableName: String = "funcAround",
  arrowCallbackPrefix: String = "init_peer_callable_"
) {

  private val returnVar: String = "-return-"

  def returnCallback(retModel: List[ValueModel]): FuncOp =
    callback(
      respFuncName,
      Call(
        retModel,
        Nil
      )
    )

  def arrowToCallback(name: String, arrowType: ArrowType): FuncCallable = {
    val (args, call, ret) = ArgsCall.arrowToArgsCallRet(arrowType)
    FuncCallable(
      arrowCallbackPrefix + name,
      callback(name, call),
      arrowType,
      ret.map(_.model),
      Map.empty,
      Map.empty
    )
  }

  def wrap(func: FuncCallable): FuncCallable = {
    val returnType = ProductType(func.ret.map(_.lastType).map {
      // we mustn't return a stream in response callback to avoid pushing stream to `-return-` value
      case StreamType(t) => ArrayType(t)
      case t => t
    }).toLabelledList(returnVar)

    val retModel = returnType.map { case (l, t) => VarModel(l, t) }

    val funcCall = Call(
      func.arrowType.domain.toLabelledList().map(ad => VarModel(ad._1, ad._2)),
      returnType.map { case (l, t) => Call.Export(l, t) }
    )

    FuncCallable(
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

  def resolve(
    func: FuncCallable,
    funcArgName: String = "_func"
  ): Eval[FuncOp] =
    wrap(func)
      .resolve(
        Call(VarModel(funcArgName, func.arrowType) :: Nil, Nil),
        Map(funcArgName -> func),
        Set.empty
      )
      .map(_._1)
}
