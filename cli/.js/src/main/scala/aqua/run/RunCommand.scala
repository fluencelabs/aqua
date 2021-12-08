package aqua.run

import aqua.*
import aqua.ErrorRendering.showError
import aqua.backend.air.{AirBackend, FuncAirGen}
import aqua.backend.js.JavaScriptBackend
import aqua.backend.ts.TypeScriptBackend
import aqua.backend.{FunctionDef, Generated}
import aqua.compiler.{AquaCompiled, AquaCompiler}
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.io.{AquaFileError, OutputPrinter}
import aqua.js.*
import aqua.model.func.raw.{CallArrowTag, CallServiceTag, FuncOp, FuncOps}
import aqua.model.func.{Call, FuncCallable}
import aqua.model.transform.res.{AquaRes, FuncRes}
import aqua.model.transform.{Transform, TransformConfig}
import aqua.model.{AquaContext, LiteralModel, ValueModel, VarModel}
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.Literal
import aqua.parser.lift.FileSpan
import aqua.run.RunConfig
import aqua.types.*
import cats.data.*
import cats.effect.kernel.{Async, Clock}
import cats.effect.syntax.async.*
import cats.effect.{IO, IOApp, Resource, Sync}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.list.*
import cats.syntax.monad.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import cats.{Id, Monad, ~>}
import fs2.io.file.{Files, Path}
import scribe.Logging

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.Dynamic.global as g
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.*

object RunCommand extends Logging {

  def keyPairOrNull(sk: Option[Array[Byte]]): Future[KeyPair] = {
    sk.map { arr =>
      val typedArr = js.typedarray.Uint8Array.from(arr.map(_.toShort).toJSArray)
      KeyPair.fromEd25519SK(typedArr).toFuture
    }.getOrElse(Future.successful[KeyPair](null))
  }

  /**
   * Calls an air code with FluenceJS SDK.
   * @param multiaddr relay to connect to
   * @param air code to call
   * @return
   */
  def funcCall[F[_]: Async](
    multiaddr: String,
    air: String,
    functionDef: FunctionDef,
    config: RunConfig,
    consoleService: ConsoleService
  )(implicit
    ec: ExecutionContext
  ): F[Unit] = {
    FluenceUtils.setLogLevel(LogLevelTransformer.logLevelToFluenceJS(config.logLevel))

    // stops peer in any way at the end of execution
    val resource = Resource.make(Fluence.getPeer().pure[F]) { peer =>
      Async[F].fromFuture(Sync[F].delay(peer.stop().toFuture))
    }

    resource.use { peer =>
      Async[F].fromFuture {
        (for {
          secretKey <- keyPairOrNull(config.secretKey)
          _ <- Fluence
            .start(
              PeerConfig(multiaddr, config.timeout, LogLevelTransformer.logLevelToAvm(config.logLevel), secretKey)
            )
            .toFuture
          _ = OutputPrinter.print("Your peerId: " + peer.getStatus().peerId)
          promise = Promise.apply[Unit]()
          _ = consoleService.registerService(peer, promise)
          _ = config.services.values.map(_.registerService(peer))
          callFuture = CallJsFunction.funcCallJs(
            air,
            functionDef,
            List.empty
          )
          _ <- Future.firstCompletedOf(promise.future :: callFuture :: Nil)
        } yield {}).recover(handleFuncCallErrors).pure[F]
      }
    }
  }

  private def findFunction(contexts: Chain[AquaContext], funcName: String): Option[FuncCallable] =
    contexts
      .flatMap(_.exports.map(e => Chain.fromSeq(e.funcs.values.toList)).getOrElse(Chain.empty))
      .find(_.funcName == funcName)

  def createGetters(vars: List[(String, Type)], services: Map[String, ArgGetterService]): ValidatedNec[String, List[ArgGetterService]] = {
    vars.map {
      (n, argType) =>
        val serviceOp = services.get(n)
        (serviceOp, argType) match {
          // BoxTypes could be nulls
          case (None, _) => Validated.invalidNec(s"Unexcepted. There is no service for '$n' argument")
          // BoxType could be undefined, so, pass service that will return undefined for this argument
          case (Some(s), _: BoxType) if s.arg == js.undefined => Validated.validNec(s :: Nil)

          case (Some(s), _) if s.arg == js.undefined => Validated.invalidNec(s"Argument '$n' is undefined, but it's type '$argType' cannot be undefined.")
          case (Some(s), _) => Validated.validNec(s :: Nil)
        }
    }.reduce(_ combine _)
  }

  // Wrap a function that it will be called in another function, and pass results to a `print` service, i.e.:
  // func wrapFunc():
  //   res <- funcCallable(args:_*)
  //   Console.print(res)
  private def wrapCall(
    funcName: String,
    funcCallable: FuncCallable,
    args: List[ValueModel],
    config: RunConfig,
    consoleService: ConsoleService
  ): ValidatedNec[String, FuncCallable] = {
    // pass results to a printing service if an input function returns a result
    // otherwise just call it
    val body = funcCallable.arrowType.codomain.toList match {
      case Nil =>
        FuncOp.leaf(CallArrowTag(funcName, Call(args, Nil)))
      case types =>
        val (variables, exports) = types.zipWithIndex.map { case (t, idx) =>
          val name = config.resultName + idx
          (VarModel(name, t), Call.Export(name, t))
        }.unzip
        val callFuncTag =
          CallArrowTag(funcName, Call(args, exports))

        val callServiceTag = consoleService.getCallServiceTag(variables)

        FuncOps.seq(FuncOp.leaf(callFuncTag), FuncOp.leaf(callServiceTag))
    }

    val vars = args.zip(funcCallable.arrowType.domain.toList)
      .collect {
        case (VarModel(n, _, _), argType) => (n, argType)
      }

    val gettersV = createGetters(vars, config.services)

    gettersV.map { getters =>
      val gettersTags = getters.map(s => FuncOp.leaf(s.getCallServiceTag()))

      FuncCallable(
        config.functionWrapperName,
        FuncOps.seq((gettersTags :+ body):_*),
        // no arguments and returns nothing
        ArrowType(NilType, NilType),
        Nil,
        Map(funcName -> funcCallable),
        Map.empty
      )
    }
  }

  private def handleFuncCallErrors: PartialFunction[Throwable, Unit] = { t =>
    val message = if (t.getMessage.contains("Request timed out after")) {
      "Function execution failed by timeout. You can increase timeout with '--timeout' option in milliseconds or check if your code can hang while executing."
    } else JSON.stringify(t.toString)
    // TODO use custom function for error output
    OutputPrinter.error(message)
  }

  def genAirAndCall[F[_]: Async](multiaddr: String, wrapped: FuncCallable, consoleService: ConsoleService, transformConfig: TransformConfig, runConfig: RunConfig)(implicit ec: ExecutionContext): F[Unit] = {
    val funcRes = Transform.fn(wrapped, transformConfig)
    val definitions = FunctionDef(funcRes)

    val air = FuncAirGen(funcRes).generate.show

    if (runConfig.printAir) {
      OutputPrinter.print(air)
    }

    funcCall[F](multiaddr, air, definitions, runConfig, consoleService)
  }

  /**
   * Runs a function that is located in `input` file with FluenceJS SDK. Returns no output
   * @param multiaddr relay to connect to
   * @param func function name
   * @param input path to an aqua code with a function
   * @param imports the sources the input needs
   */
  def run[F[_]: Files: AquaIO: Async](
    multiaddr: String,
    func: String,
    args: List[ValueModel],
    input: Path,
    imports: List[Path],
    runConfig: RunConfig,
    transformConfig: TransformConfig = TransformConfig()
  )(implicit ec: ExecutionContext): F[Unit] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]

    for {
      prelude <- Prelude.init()
      sources = new AquaFileSources[F](input, prelude.importPaths)
      // compile only context to wrap and call function later
      compileResult <- Clock[F].timed(
        AquaCompiler
          .compileToContext[F, AquaFileError, FileModuleId, FileSpan.F](
            sources,
            SpanParser.parser,
            transformConfig
          ).map(_.leftMap(_.map(_.show)))
      )
      (compileTime, contextV) = compileResult
      callResult <- Clock[F].timed {
        val resultV: ValidatedNec[String, F[Unit]] = contextV.andThen { contextC =>
            findFunction(contextC, func) match {
              case Some(funcCallable) =>

                val consoleService = new ConsoleService(runConfig.consoleServiceId, runConfig.printFunctionName)

                // call an input function from a generated function
                val callResult: ValidatedNec[String, F[Unit]] = wrapCall(func, funcCallable, args, runConfig, consoleService)
                  .map { wrapped =>
                  genAirAndCall[F](multiaddr, wrapped, consoleService, transformConfig, runConfig)
                }
                callResult
              case None =>
                Validated.invalidNec[String, F[Unit]](s"There is no function called '$func'")
            }
          }
        resultV.sequence
        }
      (callTime, result) = callResult
    } yield {
      logger.debug(s"Compile time: ${compileTime.toMillis}ms")
      logger.debug(s"Call time: ${callTime.toMillis}ms")
      result.fold(
        { (errs: NonEmptyChain[String]) =>
          errs.toChain.toList.foreach(err => OutputPrinter.error(err + "\n"))
        },
        identity
      )
    }
  }

}
