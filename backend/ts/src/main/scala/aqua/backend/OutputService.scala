package aqua.backend

import aqua.backend.ts.TypeScriptCommon
import aqua.definitions.*
import aqua.definitions.TypeDefinition.given
import aqua.res.ServiceRes
import aqua.types.ArrowType

import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*

case class OutputService(srv: ServiceRes, types: Types) {

  import TypeScriptCommon.*
  import types.*
  private val serviceTypes = types.serviceType(srv)

  import serviceTypes.*

  def generate: String =
    val functions = LabeledProductTypeDef(
      srv.members.map { case (n, a) => (n, ArrowTypeDef(a)) }
    )

    val serviceDef = ServiceDef(srv.defaultId.map(s => s.replace("\"", "")), functions, srv.name)

    s"""
       |${serviceTypes.generate}
       |
       |export function register${srv.name}(${typed("...args", "any")}) {
       |    registerService$$$$(
       |        args,
       |        ${serviceDef.asJson.deepDropNullValues.spaces4}
       |    );
       |}
      """.stripMargin
}
