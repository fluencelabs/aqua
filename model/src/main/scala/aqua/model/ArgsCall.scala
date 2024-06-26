/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.model

import aqua.model.ValueModel.{Ability, MutableStream}
import aqua.raw.ops.Call
import aqua.raw.value.VarRaw
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
      case ((name, _: DataType), value)
          if !streamArgs.contains(name) && !streamToImmutableArgs.contains(name) =>
        name -> value
    }.toMap

  /**
   * Ability and service arguments as mapping
   * Name of argument -> (variable passed in the call, type)
   */
  lazy val abilityArgs: Map[String, (VarModel, NamedType)] =
    zipped.collect { case ((name, _), Ability(vr, t)) =>
      name -> (vr, t)
    }.toMap

  /**
   * All renamings from ability arguments as mapping
   * Name inside function body -> name in the call context
   */
  lazy val abilityArgsRenames: Map[String, String] =
    abilityArgs.toList.foldMap { case (name, (vm, at)) =>
      AbilityType.renames(at)(name, vm.name)
    }

  /**
   * Stream arguments as mapping
   * Name of argument -> variable passed in the call
   * NOTE:  Argument is stream if it is passed as stream
   * on the call site. Type of argument in the function
   * definition does not matter.
   */
  lazy val streamArgs: Map[String, VarModel] =
    zipped.collect { case ((name, _: MutableStreamType), MutableStream(vr, _)) =>
      name -> vr
    }.toMap

  /**
   * All renamings from stream arguments as mapping
   * Name inside function body -> name in the call context
   */
  lazy val streamArgsRenames: Map[String, String] =
    streamArgs.view.mapValues(_.name).toMap

  /**
   * Stream arguments that will be used as immutable collections
   * Name of argument -> variable passed in the call
   */
  lazy val streamToImmutableArgs: Map[String, VarModel] =
    zipped.collect {
      case ((name, _: ImmutableCollectionType), vr@MutableStream(_)) =>
        name -> vr
    }.toMap

  lazy val streamToImmutableArgsWithTypes: Map[String, (VarModel, MutableStreamType)] =
    zipped.collect {
      case ((name, _: ImmutableCollectionType), vr@MutableStream(_, t)) =>
        name -> (vr, t)
    }.toMap

  /**
   * All renamings from stream arguments as mapping
   * Name inside function body -> name in the call context
   */
  lazy val streamToImmutableArgsRenames: Map[String, String] =
    streamToImmutableArgs.view.mapValues(_.name).toMap

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
