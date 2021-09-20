package aqua.backend.ts

import aqua.backend.ServiceTypes
import aqua.backend.ts.TypeScriptCommon.fnDef
import aqua.model.transform.res.ServiceRes

case class TSServiceTypes(srv: ServiceRes) extends ServiceTypes {
  import TypeScriptTypes._

  private val serviceTypeName = s"${srv.name}Def";

  def registerServiceArgs = {

    // defined arguments used in overloads below
    val peerDecl = s"${typed("peer", "FluencePeer")}";
    val serviceIdDecl = s"${typed("serviceId", "string")}";
    val serviceDecl = s"${typed("service", serviceTypeName)}"

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
        List(peerDecl, serviceIdDecl, serviceDecl)
      )
    )

    // Service registration functions has several overloads.
    // Depending on whether the the service has the default id or not
    // there would be different number of overloads
    // This variable contain defines the list of lists where
    // the outmost list describes the list of overloads
    // and the innermost one defines the list of arguments in the overload
    registerServiceArgsSource.map { x =>
      val args = x.mkString(", ")
      s"export function register${srv.name}(${args}): void;"
    }
      .mkString("\n")
  }

  def exportInterface = {
    val fnDefs = srv.members.map { case (name, arrow) =>
      s"${typed(name, fnDef(arrow))};"
    }
      .mkString("\n")

    s"""export interface ${serviceTypeName} {
       |    ${fnDefs}
       |}""".stripMargin
  }

  def generate = {
    s"""
       |$exportInterface
       |
       |$registerServiceArgs
       """
  }
}
