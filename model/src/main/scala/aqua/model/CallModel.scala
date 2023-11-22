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

  def callExport(ex: Call.Export): Export = Export(ex.name, ex.`type`)
}
