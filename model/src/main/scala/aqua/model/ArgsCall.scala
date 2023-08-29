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

  lazy val argNames: Set[String] = args
    .toLabelledList()
    .map { case (name, _) => name }
    .toSet

  lazy val dataArgs: Map[String, ValueModel] =
    zipped.collect {
      case ((name, _: DataType), value) if !streamArgs.contains(name) =>
        name -> value
    }.toMap

  lazy val abilityArgs: Map[String, (VarModel, AbilityType)] =
    zipped.collect { case ((name, _), vr @ VarModel(_, t @ AbilityType(_, _), _)) =>
      name -> (vr, t)
    }.toMap

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

  lazy val streamArgs: Map[String, VarModel] =
    zipped.collect { case ((name, _), vr @ VarModel(_, StreamType(_), _)) =>
      name -> vr
    }.toMap

  lazy val streamArgsRenames: Map[String, String] =
    streamArgs.view.mapValues(_.name).toMap

  lazy val arrowArgs: Map[String, VarModel] =
    zipped.collect { case ((name, _: ArrowType), vm: VarModel) =>
      name -> vm
    }.toMap

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
