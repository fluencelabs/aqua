package aqua.backend.ts

import aqua.model.{ServiceModel}
import aqua.types.{ArrowType, ProductType}
import aqua.model.transform.BodyConfig
import aqua.types._
import cats.syntax.show._

case class TypeScriptService(service: ServiceModel) {

  import TypeScriptCommon._

  def fnHandler(arrow: ArrowType, memberName: String) = {
    s"""
       | if (req.fnName === '${memberName}') {
       |     ${callBackExprBody(arrow, "service." + memberName)}
       | }
    """.stripMargin
  }

  def generateTypescript(conf: BodyConfig = BodyConfig()): String = {
    val twoSlashn = System.lineSeparator() + System.lineSeparator()
    val list = service.arrows.toNel.toList
    val fnHandlers = list
      .map({ case (name, arrow) =>
        fnHandler(arrow, name)
      })
      .mkString(twoSlashn)

    val fnDefs = list
      .map({ case (name, arrow) =>
        s"${name}: ${fnBodyDef(arrow)};"
      })
      .mkString("" + System.lineSeparator())

    val registerName = s"register${service.name}"

    val registerNameImpl = s"register${service.name}Impl"

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

}
