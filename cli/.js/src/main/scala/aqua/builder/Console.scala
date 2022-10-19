package aqua.builder

import aqua.backend.*
import aqua.io.OutputPrinter
import aqua.js.{CallJsFunction, FluencePeer, ServiceHandler}
import aqua.types.ScalarType
import cats.data.NonEmptyList
import scribe.Logging

import scala.scalajs.js
import scala.scalajs.js.JSON

private case class Console(serviceId: String, functions: NonEmptyList[AquaFunction])
    extends Service(serviceId, functions)

object Console extends Logging {

  private def printFunction(funcName: String) = new AquaFunction {
    override def fnName: String = funcName

    def handler: ServiceHandler = { varArgs =>
      js.typeOf(varArgs(0)) match {
        case "string" | "number" | "boolean" => logger.info(varArgs(0).toString)
        case _ => println(JSON.stringify(varArgs(0), space = 2))
      }
      js.Promise.resolve(Service.emptyObject)
    }

    def arrow: ArrowTypeDef = ArrowTypeDef(
      LabeledProductTypeDef(("str", ScalarTypeDef.fromScalar(ScalarType.string)) :: Nil),
      NilTypeDef
    )
  }

  val PrintName = "print"

  def apply(serviceId: String = "run-console"): Console = {

    Console(serviceId, NonEmptyList.one(printFunction(PrintName)))
  }
}
