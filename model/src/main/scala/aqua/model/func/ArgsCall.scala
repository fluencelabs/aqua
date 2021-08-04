package aqua.model.func

import aqua.model.{ValueModel, VarModel}
import aqua.types.{ArrowType, DataType, ProductType, Type}

/**
 * Wraps argument definitions of a function, along with values provided when this function is called
 * @param args Argument definitions
 * @param callWith Values provided for arguments
 */
case class ArgsCall(args: ProductType, callWith: List[ValueModel]) {
  // Both arguments (arg names and types how they seen from the function body)
  // and values (value models and types how they seen on the call site)
  lazy val zipped: List[((String, Type), ValueModel)] = args.toLabelledList() zip callWith

  lazy val dataArgs: Map[String, ValueModel] =
    zipped.collect { case ((name, _: DataType), value) =>
      name -> value
    }.toMap

  def arrowArgs(arrowsInScope: Map[String, FuncCallable]): Map[String, FuncCallable] =
    zipped.collect {
      case ((name, _: ArrowType), VarModel(value, _, _)) if arrowsInScope.contains(value) =>
        name -> arrowsInScope(value)
    }.toMap
}

object ArgsCall {

  def arrowToArgsCallRet(
    arrow: ArrowType,
    argPrefix: String = "arg",
    retName: String = "init_call_res"
  ): (ProductType, Call, Option[Call.Export]) = {
    val argNamesTypes = arrow.domain.toLabelledList(argPrefix)
    val res = arrow.codomain.toList.headOption

    val call = Call(
      argNamesTypes.map { case (a, t) =>
        VarModel(a, t)
      },
      res.map(Call.Export(retName, _))
    )

    (arrow.domain, call, arrow.res.map(t => Call.Export(retName, t)))
  }

}
