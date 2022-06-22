package aqua.run

import aqua.{CliFunc, VarJson}
import aqua.backend.{FunctionDef, TypeDefinition}
import aqua.backend.air.FuncAirGen
import aqua.builder.{ArgumentGetter, Finisher, ResultPrinter}
import aqua.io.OutputPrinter
import aqua.js.{Conversions, TypeDefinitionJs}
import cats.data.Validated.{invalidNec, validNec}
import aqua.model.transform.{Transform, TransformConfig}
import aqua.model.{FuncArrow, ValueModel, VarModel}
import aqua.raw.ops.{Call, CallArrowRawTag, FuncOp, SeqTag}
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.types.*
import cats.data.{Validated, ValidatedNec}
import cats.effect.kernel.Async
import cats.syntax.applicative.*
import cats.syntax.show.*
import cats.syntax.flatMap.*
import cats.syntax.traverse.*
import cats.syntax.partialOrder.*

import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext
import scala.scalajs.js
import scala.scalajs.js.JSON

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
  import aqua.types.Type.typesPartialOrder

  // Compare and validate type generated from JSON and type from Aqua file.
  // Also, validation will be success if array or optional field will be missed in JSON type
  def validateTypes(name: String, lt: Type, rtOp: Option[Type]): ValidatedNec[String, Unit] = {
    rtOp match {
      case None =>
        lt match {
          case tb: BoxType =>
            validNec(())
          case _ =>
            invalidNec(s"Missing field '$name' in arguments")
        }
      case Some(rt) =>
        (lt, rt) match {
          case (l: StructType, r: StructType) =>
            val lsm: SortedMap[String, Type] = l.fields.toSortedMap
            val rsm: SortedMap[String, Type] = r.fields.toSortedMap

            lsm.map { case (n, ltt) =>
              validateTypes(s"$name.$n", ltt, rsm.get(n))
            }.toList.sequence.map(_ => ())
          case (l: BoxType, r: BoxType) =>
            validateTypes(name, l.element, Some(r.element))
          case (l: BoxType, r) =>
            validateTypes(name, l.element, Some(r))
          case (l, r) =>
            if (l >= r) validNec(())
            else
              invalidNec(
                s"Type of the field '$name' is incorrect. Expected: '$l' Actual: '$r'"
              )
        }
    }

  }

  def validateArguments(
    funcDomain: List[(String, Type)],
    args: List[ValueRaw]
  ): ValidatedNec[String, Unit] = {
    if (funcDomain.size != args.length) {
      invalidNec(
        s"Number of arguments for the function is incorrect. Expected: ${args.length}. Actual: ${funcDomain.size}"
      )
    } else {
      funcDomain
        .zip(args)
        .map { case ((name, lt), rt) =>
          rt match {
            case VarRaw(n, t) =>
              validateTypes(n, lt, Some(rt.`type`))
            case _ =>
              validateTypes(name, lt, Some(rt.`type`))
          }

        }
        .sequence
        .map(_ => ())
    }
  }

  // Wraps function with necessary services, registers services and calls wrapped function with FluenceJS
  def run[F[_]: Async](): F[ValidatedNec[String, Unit]] = {
    validateArguments(
      funcCallable.arrowType.domain.labelledData,
      func.args
    ) match {
      case Validated.Valid(_) =>
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
          case Validated.Valid((wrapped, getters)) =>
            genAirAndMakeCall[F](
              wrapped,
              resultPrinterService,
              promiseFinisherService,
              getters
            )
          case i @ Validated.Invalid(_) => i.pure[F]
        }
      case v @ Validated.Invalid(_) =>
        v.pure[F]
    }

  }

  // Generates air from function, register all services and make a call through FluenceJS
  private def genAirAndMakeCall[F[_]: Async](
    wrapped: FuncArrow,
    consoleService: ResultPrinter,
    finisherService: Finisher,
    getters: List[ArgumentGetter]
  ): F[ValidatedNec[String, Unit]] = {
    // TODO: prob we can turn this Eval into F
    val funcRes = Transform.funcRes(wrapped, transformConfig).value
    val definitions = FunctionDef(funcRes)

    val air = FuncAirGen(funcRes).generate.show

    if (config.common.flags.printAir) {
      OutputPrinter.print(air)
    }

    FuncCaller.funcCall[F](
      air,
      definitions,
      config,
      finisherService,
      config.services :+ consoleService,
      getters
    )
  }

  private def createGetter(value: VarRaw, arg: js.Dynamic, argType: Type): ArgumentGetter = {
    val converted = Conversions.ts2aqua(arg, TypeDefinitionJs(TypeDefinition(argType)))
    ArgumentGetter(value.copy(baseType = argType), converted)
  }

  // Creates getter services for variables. Return an error if there is no variable in services
  // and type of this variable couldn't be optional
  private def getGettersForVars(
    vars: List[(String, Type)],
    argGetters: Map[String, VarJson]
  ): ValidatedNec[String, List[ArgumentGetter]] = {
    vars.map { (n, argType) =>
      val argGetterOp = argGetters.get(n)
      (argGetterOp, argType) match {
        case (None, _) => Validated.invalidNec(s"Unexcepted. There is no service for '$n' argument")
        // BoxType could be undefined, so, pass service that will return 'undefined' for this argument
        case (Some(s), _: BoxType) if s._2 == js.undefined =>
          Validated.validNec(createGetter(s._1, s._2, argType) :: Nil)
        case (Some(s), _) if s._2 == js.undefined =>
          Validated.invalidNec(
            s"Argument '$n' is missing. Expected argument '$n' of type '$argType'"
          )
        case (Some(s), _) =>
          Validated.validNec(createGetter(s._1, s._2, argType) :: Nil)
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
  ): ValidatedNec[String, (FuncArrow, List[ArgumentGetter])] = {
    val codomain = funcCallable.arrowType.codomain.toList
    // pass results to a printing service if an input function returns a result
    // otherwise just call it
    val body = codomain match {
      case Nil =>
        CallArrowRawTag.func(func.name, Call(func.args, Nil)).leaf
      case types =>
        val (variables, exports) = types.zipWithIndex.map { case (t, idx) =>
          val name = config.resultName + idx
          (VarRaw(name, t), Call.Export(name, t))
        }.unzip
        val callFuncTag =
          CallArrowRawTag.func(func.name, Call(func.args, exports))

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

      (
        FuncArrow(
          config.functionWrapperName,
          SeqTag.wrap((gettersTags :+ body :+ finisherServiceTag.leaf): _*),
          // no arguments and returns "ok" string
          ArrowType(NilType, returnCodomain),
          ret,
          Map(func.name -> funcCallable),
          Map.empty,
          None
        ),
        getters
      )
    }
  }

}
