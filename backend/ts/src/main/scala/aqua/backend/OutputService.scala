package aqua.backend

import aqua.backend.ts.TypeScriptCommon.callBackExprBody
import aqua.backend.ts.TypeScriptCommon
import aqua.model.transform.res.ServiceRes
import aqua.types.ArrowType

case class OutputService(srv: ServiceRes, types: Types) {

  import TypeScriptCommon.*
  import types.*
  val serviceTypes = types.serviceType(srv)
  import serviceTypes.*

  def generate: String =
    val membersNames = srv.members.map(_._1)

    s"""
      |${serviceTypes.generate}
      |
      |export function register${srv.name}(${typed("...args", "any")}) {
      |    regService({
      |        rawFnArgs: args,
      |        serviceFunctionTypes: [{}],
      |    });
      |}
      """.stripMargin
}
