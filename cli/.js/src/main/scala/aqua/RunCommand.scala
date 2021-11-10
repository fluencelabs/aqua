package aqua

import aqua.backend.air.{AirBackend, FuncAirGen}
import aqua.backend.js.JavaScriptBackend
import aqua.backend.ts.TypeScriptBackend
import aqua.backend.{FunctionDef, Generated}
import aqua.compiler.{AquaCompiled, AquaCompiler}
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.io.AquaFileError
import aqua.model.func.{Call, FuncCallable}
import aqua.model.func.raw.{CallArrowTag, CallServiceTag, FuncOp, FuncOps}
import aqua.model.transform.{Transform, TransformConfig}
import aqua.model.transform.res.{AquaRes, FuncRes}
import aqua.model.{AquaContext, LiteralModel, VarModel}
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.Literal
import aqua.parser.lift.FileSpan
import aqua.types.{ArrowType, NilType, ScalarType}
import cats.data.*
import cats.effect.kernel.{Async, Clock}
import cats.effect.syntax.async.*
import cats.effect.{IO, IOApp, Sync}
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
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.*

object RunCommand extends Logging {

  val consoleServiceId = "--console--"
  val printFunction = "print"

  /**
   * Calls an air code with FluenceJS SDK.
   * @param multiaddr relay to connect to
   * @param air code to call
   * @return
   */
  def funcCall(multiaddr: String, air: String, functionDef: FunctionDef)(implicit
    ec: ExecutionContext
  ): Future[Unit] = {
    (for {
      _ <- Fluence
        .start(PeerConfig(connectTo = multiaddr))
        .toFuture
      peer = Fluence.getPeer()
      promise = Promise.apply[Unit]()
      _ = CallJsFunction.registerUnitService(
        peer,
        consoleServiceId,
        printFunction,
        args => {
          promise.success(())
          println("result: " + args)
        }
      )
      result = CallJsFunction.funcCallJs(
        air,
        functionDef,
        List.empty
      )
      _ <- Future.firstCompletedOf(promise.future :: result :: Nil)
      _ <- peer.stop().toFuture
    } yield {})
  }

  val generatedFuncName = "callerUniqueFunction"

  def findFunction(contexts: Chain[AquaContext], funcName: String): Option[FuncCallable] =
    contexts
      .flatMap(_.exports.map(e => Chain.fromSeq(e.funcs.values.toList)).getOrElse(Chain.empty))
      .find(_.funcName == funcName)

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
    args: List[Literal[Id]],
    input: Path,
    imports: List[Path],
    config: TransformConfig = TransformConfig()
  )(implicit ec: ExecutionContext): F[Unit] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]

    val sources = new AquaFileSources[F](input, imports)

    for {
      compileResult <- Clock[F].timed(
        AquaCompiler
          .compileToContext[F, AquaFileError, FileModuleId, FileSpan.F](
            sources,
            SpanParser.parser,
            config
          )
      )
      (compileTime, contextV) = compileResult
      callResult <- Clock[F].timed {
        contextV match {
          case Validated.Valid(contextC: Chain[AquaContext]) =>
            // find
            findFunction(contextC, func).map { funcCallable =>
              /*
                make funcRes as function like:

                func someFunc():
                  res <- funcResFunc(args:_*)
                  Console.print(res)

                and then transform
               */
              val argsModel = args.map(l => LiteralModel(l.value, l.ts))
              val body = funcCallable.arrowType.res match {
                case Some(rt) =>
                  val resultName = "res"
                  val callFuncTag =
                    CallArrowTag(func, Call(argsModel, List(Call.Export(resultName, rt))))

                  val callServiceTag = CallServiceTag(
                    LiteralModel.quote(consoleServiceId),
                    printFunction,
                    Call(List(VarModel(resultName, rt)), Nil)
                  )

                  FuncOps.seq(FuncOp.leaf(callFuncTag), FuncOp.leaf(callServiceTag))
                case None =>
                  FuncOp.leaf(CallArrowTag(func, Call(argsModel, Nil)))
              }

              val fCallable = FuncCallable(
                "someFuncToRun",
                body,
                ArrowType(NilType, NilType),
                Nil,
                Map(func -> funcCallable),
                Map.empty
              )

              val funcRes = Transform.fn(fCallable, config)

              val definitions = FunctionDef(funcRes)
              val air = FuncAirGen(funcRes).generate.show
              Async[F].fromFuture {
                funcCall(multiaddr, air, definitions).pure[F]
              }.map { res =>
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
          errs.toChain.toList.foreach(err => println(err + "\n"))
        },
        identity
      )
    }
  }

}
