package aqua.run

import aqua.CliFunc
import aqua.backend.FunctionDef
import aqua.backend.air.FuncAirGen
import aqua.builder.{ArgumentGetter, Finisher, ResultPrinter}
import aqua.io.OutputPrinter
import aqua.model.transform.{Transform, TransformConfig}
import aqua.model.{FuncArrow, ValueModel, VarModel}
import aqua.raw.ops.{Call, CallArrowTag, FuncOp, SeqTag}
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.types.*
import cats.data.{Validated, ValidatedNec}
import cats.effect.kernel.Async
import cats.syntax.applicative.*
import cats.syntax.show.*
import cats.syntax.traverse.*

import scala.concurrent.ExecutionContext
import scala.scalajs.js

class Runner(
  func: CliFunc,
  funcCallable: FuncArrow,
  config: RunConfig,
  transformConfig: TransformConfig
) {

  def resultVariableNames(funcCallable: FuncArrow, name: String): List[String] =
    funcCallable.arrowType.codomain.toList.zipWithIndex.map { case (t, idx) =>
      name + idx
    }

  // Wraps function with necessary services, registers services and calls wrapped function with FluenceJS
  def run[F[_]: Async](): F[ValidatedNec[String, Unit]] = {
    val resultNames = resultVariableNames(funcCallable, config.resultName)
    val resultPrinterService =
      ResultPrinter(config.resultPrinterServiceId, config.resultPrinterName, resultNames)
    val promiseFinisherService =
      Finisher(config.finisherServiceId, config.finisherFnName)

    val wrappedV = wrapCall(
      resultPrinterService,
      promiseFinisherService
    )

    // call an input function from a generated function
    wrappedV match {
      case Validated.Valid(wrapped) =>
        genAirAndMakeCall[F](
          wrapped,
          resultPrinterService,
          promiseFinisherService
        )
      case i @ Validated.Invalid(_) => i.pure[F]
    }
  }

  // Generates air from function, register all services and make a call through FluenceJS
  private def genAirAndMakeCall[F[_]: Async](
    wrapped: FuncArrow,
    consoleService: ResultPrinter,
    finisherService: Finisher
  ): F[ValidatedNec[String, Unit]] = {
    // TODO: prob we can turn this Eval into F
    val funcRes = Transform.funcRes(wrapped, transformConfig).value
    val definitions = FunctionDef(funcRes)

    val air = FuncAirGen(funcRes).generate.show

    if (config.common.printAir) {
      OutputPrinter.print(air)
    }

    FuncCaller.funcCall[F](
      air,
      definitions,
      config,
      finisherService,
      config.services :+ consoleService
    )
  }

  // Creates getter services for variables. Return an error if there is no variable in services
  // and type of this variable couldn't be optional
  private def getGettersForVars(
    vars: List[(String, Type)],
    argGetters: Map[String, ArgumentGetter]
  ): ValidatedNec[String, List[ArgumentGetter]] = {
    vars.map { (n, argType) =>
      val argGetterOp = argGetters.get(n)
      (argGetterOp, argType) match {
        case (None, _) => Validated.invalidNec(s"Unexcepted. There is no service for '$n' argument")
        // BoxType could be undefined, so, pass service that will return 'undefined' for this argument
        case (Some(s), _: BoxType) if s.function.arg == js.undefined => Validated.validNec(s :: Nil)
        case (Some(s), _) if s.function.arg == js.undefined =>
          Validated.invalidNec(
            s"Argument '$n' is undefined, but it's type '$argType' cannot be undefined."
          )
        case (Some(s), _) => Validated.validNec(s :: Nil)
      }
    }.reduceOption(_ combine _).getOrElse(Validated.validNec(Nil))
  }

  // Wrap a functino like this:
  // func wrapFunc():
  //   arg1 <- getDataSrv()
  //   arg2 <- getDataSrv()
  //   ...
  //   res <- funcCallable(args:_*)
  //   Console.print(res)
  //   Finisher.finish()
  private def wrapCall(
    consoleService: ResultPrinter,
    finisherService: Finisher
  ): ValidatedNec[String, FuncArrow] = {
    val codomain = funcCallable.arrowType.codomain.toList
    // pass results to a printing service if an input function returns a result
    // otherwise just call it
    val body = codomain match {
      case Nil =>
        CallArrowTag(func.name, Call(func.args, Nil)).leaf
      case types =>
        val (variables, exports) = types.zipWithIndex.map { case (t, idx) =>
          val name = config.resultName + idx
          (VarRaw(name, t), Call.Export(name, t))
        }.unzip
        val callFuncTag =
          CallArrowTag(func.name, Call(func.args, exports))

        val consoleServiceTag = consoleService.callTag(variables)

        SeqTag.wrap(
          callFuncTag.leaf,
          consoleServiceTag.leaf
        )
    }

    val finisherServiceTag = finisherService.callTag()

    val vars = func.args
      .zip(funcCallable.arrowType.domain.toList)
      .collect { case (VarRaw(n, _), argType) =>
        (n, argType)
      }
      .distinctBy(_._1)

    val gettersV = getGettersForVars(vars, config.argumentGetters)

    gettersV.map { getters =>
      val gettersTags = getters.map(s => s.callTag().leaf)

      // return something to wait a result if we have return value in function
      // this is needed to catch an error if it will be occurred
      val (returnCodomain, ret) = if (codomain.isEmpty) {
        (NilType, Nil)
      } else {
        (UnlabeledConsType(ScalarType.string, NilType), LiteralRaw.quote("ok") :: Nil)
      }

      FuncArrow(
        config.functionWrapperName,
        SeqTag.wrap((gettersTags :+ body :+ finisherServiceTag.leaf): _*),
        // no arguments and returns "ok" string
        ArrowType(NilType, returnCodomain),
        ret,
        Map(func.name -> funcCallable),
        Map.empty,
        None
      )
    }
  }

}
