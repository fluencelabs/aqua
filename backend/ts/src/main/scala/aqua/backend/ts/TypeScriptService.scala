package aqua.backend.ts

import aqua.backend.air.FuncAirGen
import aqua.model.transform.res.FuncRes
import aqua.types.*
import cats.syntax.show.*
import aqua.model.transform.res.ServiceRes

case class TypeScriptService(srv: ServiceRes) {

  import TypeScriptCommon._

  def fnHandler(arrow: ArrowType, memberName: String) = {
    s"""
       | if (req.fnName === '${memberName}') {
       |     ${callBackExprBody(arrow, "service." + memberName)}
       | }
    """.stripMargin
  }

  def generate: String =
    val list = srv.members

    val fnHandlers = list
      .map({ case (name, arrow) =>
        fnHandler(arrow, name)
      })
      .mkString("\n\n")

    val fnDefs = list
      .map({ case (name, arrow) =>
        s"${name}: ${fnDef(arrow)};"
      })
      .mkString("\n")

    val registerName = s"register${srv.name}"

    val registerNameImpl = s"register${srv.name}Impl"

    val registerServiceType = s""" (
        |     options: { serviceId: string },
        |        service: {
        |            ${fnDefs}
        |        },
        | )"""

    s"""
      | const ${registerNameImpl} = (peer: FluencePeer) => {
      |     return ${registerServiceType} => {
      |           peer.callServiceHandler.use((req, resp, next) => {
      |               if (req.serviceId !== options.serviceId) {
      |                   next();
      |                   return;
      |               }
      |       
      |               ${fnHandlers}
      |       
      |               next();
      |           });
      |       }
      | }
      | 
      | export const ${registerName} = ${registerNameImpl}(FluencePeer.default);
      | 
      | declare module "@fluencelabs/fluence" {
      |     interface FluencePeer {
      |         ${registerName}: ${registerServiceType} => void;
      |     }
      | }
      |
      | FluencePeer.prototype.${registerName} = function (o, s) {
      |    return ${registerNameImpl}(this)(o, s);
      | };

      """.stripMargin
}
