package aqua.backend.js

import aqua.backend.air.FuncAirGen
import aqua.model.transform.res.FuncRes
import aqua.types.*
import cats.syntax.show.*
import aqua.model.transform.res.ServiceRes

case class JavaScriptService(srv: ServiceRes) {

  import JavaScriptCommon._

  def fnHandler(arrow: ArrowType, memberName: String) = {
    s"""
       | if (req.fnName === '${memberName}') {
       |     ${callBackExprBody(arrow, "service." + memberName)}
       | }
    """.stripMargin
  }

  def generate: String =
    val fnHandlers = srv.members
      .map{ case (name, arrow) =>
        fnHandler(arrow, name)
      }
      .mkString("\n\n")

    val registerName = s"register${srv.name}"

    val defaultServiceIdBranch = srv.defaultId.fold("")(x => 
      s""" 
      | else {
      |     serviceId = ${x}
      |}""".stripMargin
    )

    s"""
      | export function ${registerName}(...args) {
      |    let peer;
      |    let serviceId;
      |    let service;
      |    if (args[0] instanceof FluencePeer) {
      |        peer = args[0];
      |    } else {
      |        peer = FluencePeer.default;
      |    }
      |
      |    if (typeof args[0] === 'string') {
      |        serviceId = args[0];
      |    } else if (typeof args[1] === 'string') {
      |        serviceId = args[1];
      |    } ${defaultServiceIdBranch}
      |
      |    if (!(args[0] instanceof FluencePeer) && typeof args[0] === 'object') {
      |        service = args[0];
      |    } else if (typeof args[1] === 'object') {
      |        service = args[1];
      |    } else {
      |        service = args[2];
      |    }
      |
      |      peer.internals.callServiceHandler.use(async (req, resp, next) => {
      |          if (req.serviceId !== serviceId) {
      |              await next();
      |              return;
      |          }
      |  
      |          ${fnHandlers}
      |  
      |          await next();
      |      });
      | }
      """.stripMargin
}
