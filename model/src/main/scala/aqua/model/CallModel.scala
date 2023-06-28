package aqua.model

import aqua.raw.ops.Call
import aqua.types.{ArrowType, ScopeType, StructType, Type}

// TODO docs
case class CallModel(args: List[ValueModel], exportTo: List[CallModel.Export]) {
  override def toString: String = s"[${args.mkString(" ")}] ${exportTo.mkString(" ")}"

  def arrowArgNames: Set[String] = args.collect { case VarModel(m, _: ArrowType, _) =>
    m
  }.toSet

  def abilityArgs: List[(String, ScopeType)] = args.collect { case VarModel(m, t: ScopeType, _) =>
    (m, t)
  }

  def usesVarNames: Set[String] = args.flatMap(_.usesVarNames).toSet
}

object CallModel {

  case class Export(name: String, `type`: Type) {
    
    def asVar: VarModel = VarModel(name, `type`)

    override def toString: String = s"$name:${`type`}"
  }

  def callExport(ex: Call.Export): Export = Export(ex.name, ex.`type`)
}
