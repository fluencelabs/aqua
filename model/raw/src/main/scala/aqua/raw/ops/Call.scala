package aqua.raw.ops

import aqua.errors.Errors.internalError
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{ArrowType, ProductType, Type}

// TODO docs
case class Call(args: List[ValueRaw], exportTo: List[Call.Export]) {

  def mapValues(f: ValueRaw => ValueRaw): Call =
    Call(
      args.map(_.map(f)),
      exportTo
    )

  // TODO docs
  def mapExport(f: String => String): Call = copy(exportTo = exportTo.map(_.mapName(f)))

  def arrowType: ArrowType = ArrowType(
    ProductType(args.map(_.`type`)),
    ProductType.labelled(exportTo.map(e => e.name -> e.`type`))
  )

  override def toString: String =
    s"[${args.mkString(" ")}]${exportTo.map(_.toRaw).map(" " + _).mkString(",")}"
}

object Call {

  // TODO docs
  case class Export(name: String, `type`: Type, isExistingStream: Boolean = false) {
    def mapName(f: String => String): Export = copy(f(name))

    def toRaw: VarRaw = VarRaw(name, `type`)

    override def toString: String = s"$name:${`type`} <-"
  }
  
  extension (exportTo: List[Call.Export]) {
    def mapStreams(f: ValueRaw => ValueRaw): List[Call.Export] =
      exportTo.flatMap {
        // map streams from "exportTo", because they are not exports, but variables
        case ce@Call.Export(_, _, true) =>
          f(ce.toRaw) match {
            case VarRaw(name, baseType) => Some(Call.Export(name, baseType, true))
            case _ => internalError(s"Stream '$ce' can be only VarRaw")
          }
        case ce => Some(ce)
      }

    def renameExports(map: Map[String, String]): List[Call.Export] =
      exportTo.map {
        case ce@Call.Export(_, _, true) => ce
        case ce => ce.mapName(n => map.getOrElse(n, n))
      }
  }
}
