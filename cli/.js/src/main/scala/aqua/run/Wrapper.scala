package aqua.run

import aqua.builder.{ConsoleServiceBuilder, FinisherBuilder, GetterBuilder}
import aqua.model.{ValueModel, VarModel}
import aqua.model.func.{Call, FuncCallable}
import aqua.model.func.raw.{CallArrowTag, FuncOp, FuncOps}
import aqua.types.{ArrowType, BoxType, NilType, Type}
import cats.data.{Validated, ValidatedNec}

import scala.scalajs.js

// Wraps function with service calls to run it with variables and output printing
object Wrapper {

  // Creates getter services for variables. Return an error if there is no variable in services
  // and type of this variable couldn't be optional
  private def createGetters(
    vars: List[(String, Type)],
    services: Map[String, GetterBuilder]
  ): ValidatedNec[String, List[GetterBuilder]] = {
    vars.map { (n, argType) =>
      val serviceOp = services.get(n)
      (serviceOp, argType) match {
        case (None, _) => Validated.invalidNec(s"Unexcepted. There is no service for '$n' argument")
        // BoxType could be undefined, so, pass service that will return 'undefined' for this argument
        case (Some(s), _: BoxType) if s.arg == js.undefined => Validated.validNec(s :: Nil)
        case (Some(s), _) if s.arg == js.undefined =>
          Validated.invalidNec(
            s"Argument '$n' is undefined, but it's type '$argType' cannot be undefined."
          )
        case (Some(s), _) => Validated.validNec(s :: Nil)
      }
    }.reduce(_ combine _)
  }

  // Wrap a function that it will be called in another function, and pass results to a `print` service, i.e.:
  // func wrapFunc():
  //   res <- funcCallable(args:_*)
  //   Console.print(res)
  def wrapCall(
    funcName: String,
    funcCallable: FuncCallable,
    args: List[ValueModel],
    config: RunConfig,
    consoleService: ConsoleServiceBuilder,
    promiseFinisherService: FinisherBuilder
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

        val consoleServiceTag = consoleService.getCallServiceTag(variables)
        val finisherServiceTag = promiseFinisherService.getCallServiceTag()

        FuncOps.seq(
          FuncOp.leaf(callFuncTag),
          FuncOp.leaf(consoleServiceTag),
          FuncOp.leaf(finisherServiceTag)
        )
    }

    val vars = args.zip(funcCallable.arrowType.domain.toList).collect {
      case (VarModel(n, _, _), argType) => (n, argType)
    }

    val gettersV = createGetters(vars, config.argumentGetters)

    gettersV.map { getters =>
      val gettersTags = getters.map(s => FuncOp.leaf(s.getCallServiceTag()))

      FuncCallable(
        config.functionWrapperName,
        FuncOps.seq((gettersTags :+ body): _*),
        // no arguments and returns nothing
        ArrowType(NilType, NilType),
        Nil,
        Map(funcName -> funcCallable),
        Map.empty
      )
    }
  }
}
