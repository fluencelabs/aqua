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
    val fnHandlers = srv.members
      .map{ case (name, arrow) =>
        fnHandler(arrow, name)
      }
      .mkString("\n\n")

    val fnDefs = srv.members
      .map{ case (name, arrow) =>
        s"${name}: ${fnDef(arrow)};"
      }
      .mkString("\n")

    val serviceTypeName = s"${srv.name}Def";

    val registerName = s"register${srv.name}"

    // defined arguments used in overloads below
    val peerDecl = "peer: FluencePeer";
    val serviceIdDecl = "serviceId: string";
    val serviceDecl = s"service: ${serviceTypeName}"
    
    // Service registration functions has several overloads.
    // Depending on whether the the service has the default id or not
    // there would be different number of overloads
    // This variable contain defines the list of lists where 
    // the outmost list describes the list of overloads
    // and the innermost one defines the list of arguments in the overload
    val registerServiceArgsSource = srv.defaultId.fold(
      List(
        List(serviceIdDecl, serviceDecl),
        List(peerDecl, serviceIdDecl, serviceDecl)
      )
    )(_ => 
      List(
        List(serviceDecl),
        List(serviceIdDecl, serviceDecl),
        List(peerDecl, serviceDecl),
        List(peerDecl, serviceIdDecl, serviceDecl),
      )
    )

    val registerServiceArgs = registerServiceArgsSource.
      map(x => {
        val args = x.mkString(", ")
        s"export function ${registerName}(${args}): void;"
      })
      .mkString("\n");

    val defaultServiceIdBranch = srv.defaultId.fold("")(x => 
      s""" 
      | else {
      |     serviceId = ${x}
      |}""".stripMargin
    )

    s"""
      | export interface ${serviceTypeName} {
      |     ${fnDefs}
      | }
      |
      | ${registerServiceArgs}
      | export function ${registerName}(...args) {
      |    let peer: FluencePeer;
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
      |      peer.callServiceHandler.use(async (req, resp, next) => {
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
