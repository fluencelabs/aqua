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
   * @param serviceType type of the service
   * @param methodName name of the service method to wrap
   * @param methodType type of the service method to wrap
   * @param id variable that holds the service id
   * @param idValue resolved value of the service id
   * @param ret variable for return value
   */
  def fromServiceMethod(
    funcName: String,
    serviceType: ServiceType,
    methodName: String,
    methodType: ArrowType,
    id: VarRaw,
    idValue: ValueModel,
    ret: Option[VarRaw]
  ): FuncArrow = {
    val body = CallArrowRawTag.service(
      id,
      methodName,
      Call(
        methodType.domain.toLabelledList().map(VarRaw.apply),
        ret.map(r => Call.Export(r.name, r.`type`)).toList
      ),
      serviceType.name,
      methodType
    )

    FuncArrow(
      funcName = funcName,
      body = body.leaf,
      arrowType = methodType,
      ret = ret.toList,
      capturedArrows = Map.empty,
      capturedValues = Map(
        id.name -> idValue
      ),
      capturedTopology = None
    )
  }
}
