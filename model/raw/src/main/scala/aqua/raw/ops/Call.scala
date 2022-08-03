package aqua.raw.ops

import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{ArrowType, ProductType, StreamType, Type}

// TODO docs
case class Call(args: List[ValueRaw], exportTo: List[Call.Export]) {

  def mapValues(f: ValueRaw => ValueRaw): Call =
    Call(
      args.map(_.map(f)),
      exportTo
    )

  def arrowType: ArrowType = ArrowType(
    ProductType(args.map(_.`type`)),
    ProductType.labelled(exportTo.map(e => e.name -> e.`type`))
  )

  override def toString: String =
    s"[${args.mkString(" ")}]${exportTo.map(_.toRaw).map(" " + _).mkString(",")}"
}

object Call {

  // TODO docs
  case class Export(name: String, `type`: Type) {
    def mapName(f: String => String, declaredStreams: Set[String]): Export =
      `type` match {
        case StreamType(_) =>
          if (declaredStreams.contains(name)) copy(f(name)) else this
        case _ => copy(f(name))
      }

    def toRaw: VarRaw = VarRaw(name, `type`)

    override def toString: String = s"$name:${`type`} <-"
  }
}
