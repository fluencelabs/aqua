package aqua.model.transform.pre

import aqua.model.FuncArrow
import aqua.model.ArgsCall
import aqua.raw.ops.{Call, CallArrowTag, FuncOp, RawTag, SeqTag}
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.*

// TODO: doc
case class FuncPreTransformer(
  transform: RawTag.Tree => RawTag.Tree,
  callback: (String, Call) => RawTag.Tree,
  respFuncName: String,
  wrapCallableName: String = "funcAround",
  arrowCallbackPrefix: String = "init_peer_callable_"
) {

  private val returnVar: String = "-return-"

  /**
   * Wraps return values of a function to a call on itin peer's side
   *
   * @param retModel List of returned values
   * @return AST that consumes return values, passing them to the client
   */
  private def returnCallback(retModel: List[ValueRaw]): RawTag.Tree =
    callback(
      respFuncName,
      Call(
        retModel,
        Nil
      )
    )

  /**
   * Convert an arrow-type argument to init user's callback
   *
   * @param name      Argument name
   * @param arrowType Argument type
   * @return FuncArrow that can be called and delegates the call to a client-registered callback
   */
  private def arrowToCallback(name: String, arrowType: ArrowType): FuncArrow = {
    val (args, call, ret) = ArgsCall.arrowToArgsCallRet(arrowType)
    FuncArrow(
      arrowCallbackPrefix + name,
      callback(name, call),
      arrowType,
      ret.map(_.toRaw),
      Map.empty,
      Map.empty
    )
  }

  /**
   * Applies given transformations on function's Raw model,
   * removes arguments, converts them to data getters,
   * removes function return
   *
   * @param func Function to transform
   * @return
   */
  def preTransform(func: FuncArrow): FuncArrow = {
    val returnType = ProductType(func.ret.map(_.`type`).map {
      // we mustn't return a stream in response callback to avoid pushing stream to `-return-` value
      case StreamType(t) => ArrayType(t)
      case t => t
    }).toLabelledList(returnVar)

    val retModel = returnType.map { case (l, t) => VarRaw(l, t) }

    val funcCall = Call(
      func.arrowType.domain.toLabelledList().map(ad => VarRaw(ad._1, ad._2)),
      returnType.map { case (l, t) => Call.Export(l, t) }
    )

    FuncArrow(
      wrapCallableName,
      transform(
        // TODO wrapNonEmpty?
        SeqTag.wrap(
          CallArrowTag(func.funcName, funcCall).leaf ::
            returnType.headOption
              .map(_ => returnCallback(retModel))
              .toList: _*
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
}
