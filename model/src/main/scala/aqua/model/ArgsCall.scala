package aqua.model

import aqua.model.{ValueModel, VarModel}
import aqua.raw.ops.Call
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.*

/**
 * Wraps argument definitions of a function, along with values provided when this function is called
 *
 * @param args
 *   Argument definitions
 * @param callWith
 *   Values provided for arguments
 */
case class ArgsCall(args: ProductType, callWith: List[ValueModel]) {
  // Both arguments (arg names and types how they seen from the function body)
  // and values (value models and types how they seen on the call site)
  lazy val zipped: List[((String, Type), ValueModel)] = args.toLabelledList() zip callWith

  private lazy val dataArgs: Map[String, ValueModel] =
    zipped.collect { case ((name, _: DataType), value) =>
      name -> value
    }.toMap

  lazy val nonStreamDataArgs: Map[String, ValueModel] =
    dataArgs.filter { 
      case (_, VarModel(_, StreamType(_), _)) =>
       false
      case _ => true
    }

  lazy val streamArgs: Map[String, VarModel] =
    dataArgs.collect { case (k, vr @ VarModel(n, StreamType(_), _)) =>
      (k, vr)
    }

  def arrowArgs[T](arrowsInScope: Map[String, T]): Map[String, T] =
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
