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

  def fnDef(arrow: ArrowType, memberName: String) = {
    val argsWithTypes = arrow.args
      .map(typeToTs)
      .zipWithIndex
      .map(_.swap)
      .map(kv => ("arg" + kv._1, kv._2))

    val callParamsGeneric = if (argsWithTypes.length > 0) {
      val prep = argsWithTypes
        .map(kv => kv._1)
        .mkString(" | ")

      "'" + prep + "'"
    } else {
      "null"
    }

    val args = argsWithTypes
      .map(kv => kv._1 + ": " + kv._2)
      .concat(List(s"callParams: CallParams<${callParamsGeneric}>"))
      .mkString(", ")

    s"${memberName}: (${args}) => ${arrow.res.fold("void")(typeToTs)};"
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
        fnDef(arrow, name)
      })
      .mkString("" + System.lineSeparator())

    s"""
       |export function register${service.name}(
       |    client: FluenceClient,
       |    id: string,
       |    service: {
       |        ${fnDefs}
       |    },
       |) {
       |    client.callServiceHandler.use((req, resp, next) => {
       |        if (req.serviceId !== id) {
       |            next();
       |            return;
       |        }
       |
       |        ${fnHandlers}
       |
       |        next();
       |    });
       |}
    """.stripMargin
  }

}
