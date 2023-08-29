package aqua.model.transform.pre

import aqua.model.FuncArrow
import aqua.model.ArgsCall
import aqua.raw.ops.{Call, CallArrowRawTag, RawTag, SeqTag, TryTag}
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.*

import cats.syntax.show.*
import cats.syntax.option.*

// TODO: doc
case class FuncPreTransformer(
  argsProvider: ArgsProvider,
  resultsHandler: ResultsHandler,
  errorHandler: ErrorHandler,
  callback: (String, Call) => RawTag.Tree,
  relayVarName: Option[String],
  wrapCallableName: String = "funcAround",
  arrowCallbackPrefix: String = "init_peer_callable_"
) {

  private val returnVar: String = "-return-"

  private val relayVar = relayVarName.map(_ -> ScalarType.string)

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
      Map.empty,
      None
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

    val args = func.arrowType.domain.labelledData.map { case (name, typ) =>
      s"-$name-arg-" -> typ
    }

    val funcCall = Call(
      args.map { case (name, typ) => VarRaw(name, typ) },
      returnType.map { case (l, t) => Call.Export(l, t) }
    )

    val provideArgs = argsProvider.provideArgs(
      relayVar.toList ::: args
    )

    val handleResults = resultsHandler.handleResults(
      returnType
    )

    val handleError = errorHandler.handleLastError

    val call = CallArrowRawTag.func(func.funcName, funcCall).leaf

    val body = SeqTag.wrap(
      provideArgs ++ List(
        TryTag.wrap(
          call,
          handleError
        )
      ) ++ handleResults
    )

    FuncArrow(
      wrapCallableName,
      body,
      ArrowType(ConsType.cons(func.funcName, func.arrowType, NilType), NilType),
      Nil,
      func.arrowType.domain
        .toLabelledList()
        .collect { case (argName, arrowType: ArrowType) =>
          argName -> arrowToCallback(argName, arrowType)
        }
        .toMap,
      Map.empty,
      None
    )
  }
}
