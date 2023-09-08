package aqua.model

import aqua.raw.Raw
import aqua.raw.arrow.FuncRaw
import aqua.raw.ops.{Call, CallArrowRawTag, RawTag}
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{ArrowType, ServiceType, Type}

case class FuncArrow(
  funcName: String,
  body: RawTag.Tree,
  arrowType: ArrowType,
  ret: List[ValueRaw],
  capturedArrows: Map[String, FuncArrow],
  capturedValues: Map[String, ValueModel],
  capturedTopology: Option[String]
) {

  lazy val args: List[(String, Type)] = arrowType.domain.toLabelledList()

  lazy val argNames: List[String] = args.map { case (name, _) => name }

  lazy val returnedArrows: Set[String] =
    ret.collect { case VarRaw(name, _: ArrowType) => name }.toSet

}

object FuncArrow {

  def fromRaw(
    raw: FuncRaw,
    arrows: Map[String, FuncArrow],
    constants: Map[String, ValueModel],
    topology: Option[String] = None
  ): FuncArrow =
    FuncArrow(
      raw.name,
      raw.arrow.body,
      raw.arrow.`type`,
      raw.arrow.ret,
      arrows,
      constants,
      topology
    )

  /**
   * Create function - wrapper around a service method
   *
   * @param funcName name of the function
   * @param serviceName name of the service
   * @param methodName name of the service method to wrap
   * @param methodType type of the service method to wrap
   * @param idValue resolved value of the service id
   * @return `FuncArrow` wrapper for the service method
   */
  def fromServiceMethod(
    funcName: String,
    serviceName: String,
    methodName: String,
    methodType: ArrowType,
    idValue: ValueModel
  ): FuncArrow = {
    val id = VarRaw("id", idValue.`type`)
    val retVar = methodType.res.map(t => VarRaw("ret", t))

    val call = Call(
      methodType.domain.toLabelledList().map(VarRaw.apply),
      retVar.map(r => Call.Export(r.name, r.`type`)).toList
    )
    val body = CallArrowRawTag.service(
      serviceId = id,
      fnName = methodName,
      call = call,
      name = serviceName,
      arrowType = methodType
    )

    FuncArrow(
      funcName = funcName,
      body = body.leaf,
      arrowType = methodType,
      ret = retVar.toList,
      capturedArrows = Map.empty,
      capturedValues = Map(
        id.name -> idValue
      ),
      capturedTopology = None
    )
  }
}
