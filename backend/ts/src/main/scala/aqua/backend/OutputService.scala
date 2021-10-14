package aqua.backend

import aqua.backend.ts.TypeScriptCommon
import aqua.backend.ts.TypeScriptCommon.callBackExprBody
import aqua.model.transform.res.ServiceRes
import aqua.types.ArrowType
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*

case class OutputService(srv: ServiceRes, types: Types) {

  import TypeScriptCommon.*
  import types.*
  val serviceTypes = types.serviceType(srv)
  import serviceTypes.*

  def generate: String =
    val functions = srv.members.map{ m =>
      val cDef = CallbackDef(m._2)
      FunctionBodyDef(m._1, cDef.argDefs, cDef.returnType)
    }

    val serviceDef = ServiceDef(functions)

    s"""
      |${serviceTypes.generate}
      |
      |export function register${srv.name}(${typed("...args", "any")}) {
      |    registerService(
      |        args,
      |        ${serviceDef.asJson.spaces4}
      |    );
      |}
      """.stripMargin
}
