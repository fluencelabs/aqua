package aqua.raw.ops

import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{ArrowType, Type}

// TODO docs
case class Call(args: List[ValueRaw], exportTo: List[Call.Export]) {

  def mapValues(f: ValueRaw => ValueRaw): Call =
    Call(
      args.map(_.map(f)),
      exportTo
    )

  // TODO docs
  def mapExport(f: String => String): Call = copy(exportTo = exportTo.map(_.mapName(f)))

  def argVarNames: Set[String] = args.flatMap(_.usesVarNames).toSet

  override def toString: String =
    s"[${args.mkString(" ")}]${exportTo.map(_.toRaw).map(" " + _).mkString(",")}"
}

object Call {

  // TODO docs
  case class Export(name: String, `type`: Type) {
    def mapName(f: String => String): Export = copy(f(name))

    def toRaw: VarRaw = VarRaw(name, `type`)
  }
}
