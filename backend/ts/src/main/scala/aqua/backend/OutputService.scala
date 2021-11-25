package aqua.backend

import aqua.backend.ts.TypeScriptCommon
import aqua.model.transform.res.ServiceRes
import aqua.types.ArrowType
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*

case class OutputService(srv: ServiceRes, types: Types) {

  import TypeScriptCommon.*
  import types.*
  private val serviceTypes = types.serviceType(srv)
  import serviceTypes.*

  def generate: String =
    val functions = srv.members.map{ m =>
      val cDef = CallbackDefinition(m._2)
      ServiceFunctionDef(m._1, cDef.argDefs, cDef.returnType)
    }

    val serviceDef = ServiceDef(srv.defaultId.map(s => s.replace("\"", "")), functions)

    s"""
      |${serviceTypes.generate}
      |
      |export function register${srv.name}(${typed("...args", "any")}) {
      |    registerService(
      |        args,
      |        ${serviceDef.asJson.deepDropNullValues.spaces4}
      |    );
      |}
      """.stripMargin
}
