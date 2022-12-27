package aqua.run

import aqua.backend.air.FuncAirGen
import aqua.definitions.{FunctionDef, TypeDefinition}
import aqua.io.OutputPrinter
import aqua.model.transform.{Transform, TransformConfig}
import aqua.model.{FuncArrow, ValueModel, VarModel}
import aqua.parser.lexer.CallArrowToken
import aqua.parser.lift.Span
import aqua.raw.ops.{Call, CallArrowRawTag, FuncOp, SeqTag}
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.types.*
import cats.data.Validated.{invalid, invalidNec, invalidNel, validNec, validNel}
import cats.data.{NonEmptyList, Validated, ValidatedNec}
import cats.effect.kernel.Async
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.partialOrder.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import cats.{~>, Id}

import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext

// для fcli надо:
// - аргументами передать в новую функцию значения с джсона, чтоб совпадали имена
// - передать в вызываемую функцию вэлью (аргументы) и литералы
// - во внешней функции указать результатами результаты внутренней функции
//
//
//

class RunPreparer(
  func: CliFunc,
  funcCallable: FuncArrow,
  transformConfig: TransformConfig
) {

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
            case VarRaw(n, _) =>
              TypeValidator.validateTypes(n, lt, Some(rt.`type`))
            case _ =>
              TypeValidator.validateTypes(name, lt, Some(rt.`type`))
          }

        }
        .sequence
        .map(_ => ())
    }
  }

  // Wraps function with necessary services, registers services and calls wrapped function with FluenceJS
  def prepare(): ValidatedNec[String, RunInfo] = {
    validateArguments(
      funcCallable.arrowType.domain.labelledData,
      func.args
    ).map(_ =>
      genCallInfo(
        wrapCall()
      )
    )
  }

  // Generates air from function, register all services and make a call through FluenceJS
  private def genCallInfo(
    wrapped: FuncArrow
  ): RunInfo = {
    // TODO: prob we can turn this Eval into F
    val funcRes = Transform.funcRes(wrapped, transformConfig).value
    val definitions = FunctionDef(funcRes)

    val air = FuncAirGen(funcRes).generate.show

    RunInfo(func.name, air, definitions)
  }

  private def wrapCall(): FuncArrow = {
    val codomain = funcCallable.arrowType.codomain.toList
    // pass results to a printing service if an input function returns a result
    // otherwise just call it
    val (results, body) = codomain match {
      case Nil =>
        Nil -> CallArrowRawTag.func(func.name, Call(func.args, Nil)).leaf
      case types =>
        val (variables, exports) = types.zipWithIndex.map { case (t, idx) =>
          val name = func.name + "_result" + idx
          (VarRaw(name, t), Call.Export(name, t))
        }.unzip

        val callFuncTag =
          CallArrowRawTag.func(func.name, Call(func.args, exports))

        variables -> callFuncTag.leaf
    }

    val returnCodomain = ProductType(results.map(_.`type`))

    // arguments is only variables, without literals
    val argumentsType = ProductType.labelled(func.args.zip(funcCallable.arrowType.domain.labelledData).collect {
      case (VarRaw(name, _), (_, t)) => (name, t)
    })

    FuncArrow(
      func.name + "Run",
      SeqTag.wrap(body),
      ArrowType(argumentsType, returnCodomain),
      results,
      Map(func.name -> funcCallable),
      Map.empty,
      None
    )

  }

}
