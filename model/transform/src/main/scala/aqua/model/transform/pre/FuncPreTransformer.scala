/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.model.transform.pre

import aqua.model.{ArgsCall, FuncArrow}
import aqua.raw.ops.*
import aqua.raw.value.VarRaw
import aqua.types.*

import cats.syntax.option.*

/**
 * Pre-transformer for top functions:
 * - Get arguments
 * - Get relay
 * - Generate callbacks for function arguments
 * - Handle result
 * - Handle error
 *
 * @param argsProvider - provides arguments
 * @param resultsHandler - handles results
 * @param errorHandler - handles errors
 * @param callback - generates callback for function argument
 * @param relayVarName - name of the relay variable
 * @param wrapCallableName - name of the generated wrapper function
 * @param arrowCallbackPrefix - prefix for generated callbacks names
 */
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

  private val relayArg = relayVarName.map(name => ArgsProvider.Arg(name, name, ScalarType.string))

  /**
   * Convert an arrow-type argument to init user's callback
   *
   * @param name      Argument name
   * @param arrowType Argument type
   * @return FuncArrow that can be called and delegates the call to a client-registered callback
   */
  private def arrowToCallback(name: String, arrowType: ArrowType): FuncArrow = {
    val (_, call, ret) = ArgsCall.arrowToArgsCallRet(arrowType)
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
   * @return Transformed function
   */
  def preTransform(func: FuncArrow): FuncArrow = {
    val returnType = ProductType(func.ret.map(_.`type`).map {
      // we mustn't return a stream in response callback to avoid pushing stream to `-return-` value
      case StreamType(t) => ArrayType(t)
      case t => t
    }).toLabelledList(returnVar)

    /**
     * Arguments list (argument name, variable name, argument type).
     * We need to give other names to arguments because they can
     * collide with the name of the function itself.
     */
    val args = func.arrowType.domain.toLabelledList().map { case (name, typ) =>
      (name, s"-$name-arg-", typ)
    }

    val nonArrowArgs = args.collect { case (name, varName, t: (DataType | StreamType)) =>
      ArgsProvider.Arg(name, varName, t)
    }

    val arrowArgs = args.collect { case (name, argName, arrowType: ArrowType) =>
      argName -> arrowToCallback(name, arrowType)
    }.toMap

    val funcCall = Call(
      args.map { case (_, varName, t) => VarRaw(varName, t) },
      returnType.map { case (l, t) => Call.Export(l, t) }
    )

    val provideArgs = argsProvider.provideArgs(
      relayArg.toList ::: nonArrowArgs
    )

    val handleResults = resultsHandler.handleResults(
      returnType
    )

    val handleError = errorHandler.handleError

    val call = CallArrowRawTag.func(func.funcName, funcCall).leaf

    val body = TryTag.wrap(
      SeqTag.wrap(
        provideArgs
          .appended(call)
          .appendedAll(handleResults)
      ),
      handleError
    )

    FuncArrow(
      wrapCallableName,
      body,
      ArrowType(ConsType.cons(func.funcName, func.arrowType, NilType), NilType),
      Nil,
      arrowArgs,
      Map.empty,
      None
    )
  }
}
