package aqua.raw.arrow

import aqua.raw.ops.Call
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{ArrowType, DataType, ProductType, Type}

/**
 * Wraps argument definitions of a function, along with values provided when this function is called
 * @param args Argument definitions
 * @param callWith Values provided for arguments
 */
case class ArgsCall(args: ProductType, callWith: List[ValueRaw]) {
  // Both arguments (arg names and types how they seen from the function body)
  // and values (value models and types how they seen on the call site)
  lazy val zipped: List[((String, Type), ValueRaw)] = args.toLabelledList() zip callWith

  lazy val dataArgs: Map[String, ValueRaw] =
    zipped.collect { case ((name, _: DataType), value) =>
      name -> value
    }.toMap

  def arrowArgs(arrowsInScope: Map[String, FuncArrow]): Map[String, FuncArrow] =
    zipped.collect {
      case ((name, _: ArrowType), VarRaw(value, _, _)) if arrowsInScope.contains(value) =>
        name -> arrowsInScope(value)
    }.toMap
}

object ArgsCall {

  def arrowToArgsCallRet(
    arrow: ArrowType,
    argPrefix: String = "arg",
    retName: String = "init_call_res"
  ): (ProductType, Call, List[Call.Export]) = {
    val argNamesTypes = arrow.domain.toLabelledList(argPrefix)
    val res = arrow.codomain.toLabelledList(retName).map(Call.Export(_, _))

    val call = Call(
      argNamesTypes.map { case (a, t) =>
        VarRaw(a, t)
      },
      res
    )

    (arrow.domain, call, res)
  }

}
