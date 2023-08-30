package aqua.model

import aqua.model.{ValueModel, VarModel}
import aqua.raw.ops.Call
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.*

import cats.syntax.foldable.*

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
  private lazy val zipped: List[((String, Type), ValueModel)] = args.toLabelledList() zip callWith

  /**
   * Names of arguments as they defined in the function definition
   */
  lazy val argNames: Set[String] = args
    .toLabelledList()
    .map { case (name, _) => name }
    .toSet

  /**
   * Data arguments (except streams) as mapping
   * Name of argument -> value passed in the call
   */
  lazy val dataArgs: Map[String, ValueModel] =
    zipped.collect {
      case ((name, _: DataType), value) if !streamArgs.contains(name) =>
        name -> value
    }.toMap

  /**
   * Ability arguments as mapping
   * Name of argument -> (variable passed in the call, ability type)
   */
  lazy val abilityArgs: Map[String, (VarModel, AbilityType)] =
    zipped.collect { case ((name, _), vr @ VarModel(_, t @ AbilityType(_, _), _)) =>
      name -> (vr, t)
    }.toMap

  /**
   * All renamings from ability arguments as mapping
   * Name inside function body -> name in the call context
   */
  lazy val abilityArgsRenames: Map[String, String] =
    abilityArgs.toList.foldMap { case (name, (vm, at)) =>
      at.arrows.keys
        .map(arrowPath =>
          val fullName = AbilityType.fullName(name, arrowPath)
          val newFullName = AbilityType.fullName(vm.name, arrowPath)
          fullName -> newFullName
        )
        .toMap
        .updated(name, vm.name)
    }

  /**
   * Stream arguments as mapping
   * Name of argument -> variable passed in the call
   * NOTE:  Argument is stream if it is passed as stream
   *        on the call site. Type of argument in the function
   *        definition does not matter.
   */
  lazy val streamArgs: Map[String, VarModel] =
    zipped.collect { case ((name, _), vr @ VarModel(_, StreamType(_), _)) =>
      name -> vr
    }.toMap

  /**
   * All renamings from stream arguments as mapping
   * Name inside function body -> name in the call context
   */
  lazy val streamArgsRenames: Map[String, String] =
    streamArgs.view.mapValues(_.name).toMap

  /**
   * Arrow arguments as mapping
   * Name of argument -> variable passed in the call
   */
  lazy val arrowArgs: Map[String, VarModel] =
    zipped.collect { case ((name, _: ArrowType), vm: VarModel) =>
      name -> vm
    }.toMap

  /**
   * All renamings from arrow arguments as mapping
   * Name inside function body -> name in the call context
   */
  lazy val arrowArgsRenames: Map[String, String] =
    arrowArgs.view.mapValues(_.name).toMap

  def arrowArgsMap[T](arrows: Map[String, T]): Map[String, T] =
    arrowArgs.view
      .mapValues(_.name)
      .flatMap { case (name, argName) =>
        arrows.get(argName).map(name -> _)
      }
      .toMap
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
