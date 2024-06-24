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

import aqua.model.ValueModel.{Ability, Arrow}
import aqua.raw.ops.Call
import aqua.types.*

// TODO docs
case class CallModel(args: List[ValueModel], exportTo: List[CallModel.Export]) {
  override def toString: String = s"[${args.mkString(" ")}] ${exportTo.mkString(" ")}"

  def arrowArgNames: Set[String] = args.collect { case Arrow(m, _) =>
    m.name
  }.toSet

  def abilityArgs: List[(String, GeneralAbilityType)] =
    args.collect { case Ability(m, t) => m.name -> t }

  def usesVarNames: Set[String] = args.flatMap(_.usesVarNames).toSet
}

object CallModel {

  case class Export(name: String, `type`: Type) {

    def asVar: VarModel = VarModel(name, `type`)

    override def toString: String = s"$name:${`type`}"
  }

  object Export {
    def apply(vm: VarModel): Export = Export(vm.name, vm.`type`)
  }

  def callExport(ex: Call.Export): Export = Export(ex.name, ex.`type`)
}
