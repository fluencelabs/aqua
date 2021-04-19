package aqua.model.func

import aqua.model.{ValueModel, VarModel}
import aqua.types.{ArrowType, DataType}
import cats.syntax.functor._

/**
 * Wraps argument definitions of a function, along with values provided when this function is called
 * @param args Argument definitions
 * @param callWith Values provided for arguments
 */
case class ArgsCall(args: List[ArgDef], callWith: List[Call.Arg]) {
  // Both arguments (arg names and types how they seen from the function body)
  // and values (value models and types how they seen on the call site)
  lazy val zipped: List[(ArgDef, Call.Arg)] = args zip callWith

  lazy val dataArgs: Map[String, ValueModel] =
    zipped.collect { case (ArgDef.Data(name, _), Call.Arg(value, _)) =>
      name -> value
    }.toMap

  def arrowArgs(arrowsInScope: Map[String, FuncCallable]): Map[String, FuncCallable] =
    zipped.collect {
      case (ArgDef.Arrow(name, _), Call.Arg(VarModel(value, _), _))
          if arrowsInScope.contains(value) =>
        name -> arrowsInScope(value)
    }.toMap
}

object ArgsCall {

  def arrowToArgsCallRet(
    arrow: ArrowType,
    argPrefix: String = "arg",
    retName: String = "init_call_res"
  ): (ArgsDef, Call, Option[Call.Arg]) = {
    val argNamesTypes = arrow.args.zipWithIndex.map(iv => iv.map(i => argPrefix + i).swap)

    val argsDef = ArgsDef(argNamesTypes.map {
      case (a, t: DataType) => ArgDef.Data(a, t)
      case (a, t: ArrowType) => ArgDef.Arrow(a, t)
    })

    val call = Call(
      argNamesTypes.map { case (a, t) =>
        Call.Arg(VarModel(a), t)
      },
      arrow.res.as(retName)
    )

    (argsDef, call, arrow.res.map(t => Call.Arg(VarModel(retName), t)))
  }

}
