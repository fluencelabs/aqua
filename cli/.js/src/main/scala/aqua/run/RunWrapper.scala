package aqua.run

import aqua.builder.{ArgumentGetter, Console, Finisher}
import aqua.model.{ValueModel, VarModel}
import aqua.model.func.{Call, FuncCallable}
import aqua.model.func.raw.{CallArrowTag, FuncOp, FuncOps}
import aqua.types.{ArrowType, BoxType, NilType, Type}
import cats.data.{Validated, ValidatedNec}

import scala.scalajs.js

// Wraps function to run with service calls to run it with variables and output printing
object RunWrapper {

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
        case (Some(s), _: BoxType) if s.arg == js.undefined => Validated.validNec(s :: Nil)
        case (Some(s), _) if s.arg == js.undefined =>
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
  def wrapCall(
    funcName: String,
    funcCallable: FuncCallable,
    args: List[ValueModel],
    config: RunConfig,
    consoleService: Console,
    finisherService: Finisher
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

        val consoleServiceTag = consoleService.callTag(variables)

        FuncOps.seq(
          FuncOp.leaf(callFuncTag),
          FuncOp.leaf(consoleServiceTag)
        )
    }

    val finisherServiceTag = finisherService.callTag()

    val vars = args.zip(funcCallable.arrowType.domain.toList).collect {
      case (VarModel(n, _, _), argType) => (n, argType)
    }

    val gettersV = getGettersForVars(vars, config.argumentGetters)

    gettersV.map { getters =>
      val gettersTags = getters.map(s => FuncOp.leaf(s.callTag()))

      FuncCallable(
        config.functionWrapperName,
        FuncOps.seq((gettersTags :+ body :+ FuncOp.leaf(finisherServiceTag)): _*),
        // no arguments and returns nothing
        ArrowType(NilType, NilType),
        Nil,
        Map(funcName -> funcCallable),
        Map.empty
      )
    }
  }
}
