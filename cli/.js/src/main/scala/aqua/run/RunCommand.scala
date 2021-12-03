package aqua.run

import aqua.*
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
import aqua.model.{AquaContext, LiteralModel, VarModel}
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.Literal
import aqua.parser.lift.FileSpan
import aqua.run.RunConfig
import aqua.types.{ArrowType, NilType, ScalarType}
import cats.data.*
import cats.effect.kernel.{Async, Clock}
import cats.effect.syntax.async.*
import cats.effect.{IO, IOApp, Resource, Sync}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monad.*
import cats.syntax.show.*
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
    config: RunConfig
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
          _ = CallJsFunction.registerUnitService(
            peer,
            config.consoleServiceId,
            config.printFunctionName,
            args => {
              val str = JSON.stringify(args, space = 2)
              // if an input function returns a result, our success will be after it is printed
              // otherwise finish after JS SDK will finish sending a request
              // TODO use custom function for output
              OutputPrinter.print(str)
              promise.success(())
            }
          )
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

  // Wrap a function that it will be called in another function, and pass results to a `print` service, i.e.:
  // func wrapFunc():
  //   res <- funcCallable(args:_*)
  //   Console.print(res)
  private def wrapCall(
    funcName: String,
    funcCallable: FuncCallable,
    args: List[LiteralModel],
    config: RunConfig
  ): FuncCallable = {
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

        val callServiceTag = CallServiceTag(
          LiteralModel.quote(config.consoleServiceId),
          config.printFunctionName,
          Call(variables, Nil)
        )

        FuncOps.seq(FuncOp.leaf(callFuncTag), FuncOp.leaf(callServiceTag))
    }

    FuncCallable(
      config.functionWrapperName,
      body,
      // no arguments and returns nothing
      ArrowType(NilType, NilType),
      Nil,
      Map(funcName -> funcCallable),
      Map.empty
    )
  }

  private def handleFuncCallErrors: PartialFunction[Throwable, Unit] = { t =>
    val message = if (t.getMessage.contains("Request timed out after")) {
      "Function execution failed by timeout. You can increase timeout with '--timeout' option in milliseconds or check if your code can hang while executing."
    } else t.getMessage
    // TODO use custom function for error output
    OutputPrinter.error(message)
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
    args: List[LiteralModel],
    input: Path,
    imports: List[Path],
    runConfig: RunConfig,
    transformConfig: TransformConfig = TransformConfig()
  )(implicit ec: ExecutionContext): F[Unit] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]

    val prelude = Prelude.create()

    val richedImports =
      if (runConfig.noGlobalImports) imports
    else (prelude.importPaths ++ imports).distinct

    val sources = new AquaFileSources[F](input, richedImports)

    for {
      // compile only context to wrap and call function later
      compileResult <- Clock[F].timed(
        AquaCompiler
          .compileToContext[F, AquaFileError, FileModuleId, FileSpan.F](
            sources,
            SpanParser.parser,
            transformConfig
          )
      )
      (compileTime, contextV) = compileResult
      callResult <- Clock[F].timed {
        contextV match {
          case Validated.Valid(contextC: Chain[AquaContext]) =>
            findFunction(contextC, func).map { funcCallable =>
              // call an input function from a generated function
              val wrapped = wrapCall(func, funcCallable, args, runConfig)

              val funcRes = Transform.fn(wrapped, transformConfig)
              val definitions = FunctionDef(funcRes)

              val air = FuncAirGen(funcRes).generate.show

              if (runConfig.printAir) {
                OutputPrinter.print(air)
              }

              funcCall[F](multiaddr, air, definitions, runConfig).map { _ =>
                Validated.validNec(())
              }
            }.getOrElse(Validated.invalidNec(s"There is no function called '$func'").pure[F])
          case Validated.Invalid(errs) =>
            import ErrorRendering.showError
            Validated.invalid(errs.map(_.show)).pure[F]

        }
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
